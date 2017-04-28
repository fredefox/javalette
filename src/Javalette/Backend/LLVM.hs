{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Javalette.Backend.LLVM
  ( backend
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import System.IO
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

import Javalette.Backend.Internals
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint
import Javalette.Backend.LLVM.Renamer (rename)

backend :: Backend
backend = Backend
  { compiler = compile
  }

compile :: FilePath -> Jlt.Prog -> IO ()
compile fp = ioStuff . compileProg
  where
    ioStuff :: Either CompilerErr LLVM.Prog -> IO ()
    ioStuff (Left e)  = putStrLnStdErr . prettyShow $ e
    ioStuff (Right p) = do
      putStrLn (prettyShow p)
      writeFile fp (prettyShow p)

putStrLnStdErr :: String -> IO ()
putStrLnStdErr = hPutStrLn stderr

data CompilerErr = Generic String deriving (Show)
data Env = Env
  -- Each time a variable is encountered it is mapped to the next number in a
  -- sequence.
  { maxLabel :: Int
  } deriving (Show)

--incrLabel :: Compiler Int
--incrLabel :: StateT Env (Except CompilerErr) Int
incrLabel :: MonadState Env m => m Int
incrLabel = do
  lbl <- maxLabel <$> get
  modify (\e -> e { maxLabel = succ lbl })
  return lbl

newLabel :: MonadState Env m => m LLVM.Label
newLabel = LLVM.Label . ('l':) . show <$> incrLabel

newLabelNamed :: MonadCompile m => String -> m LLVM.Label
newLabelNamed s = LLVM.Label . (s ++ ) . show <$> incrLabel

emptyEnv :: Env
emptyEnv = Env 0

instance Pretty CompilerErr where
  pPrint err = case err of
    Generic s -> text "ERR:" <+> text s

type Compiler a = WriterT [AlmostInstruction] (StateT Env (Except CompilerErr)) a

type MonadCompile m =
  ( MonadWriter [AlmostInstruction] m
  , MonadState Env m
  , MonadError CompilerErr m
  )

runCompiler :: Compiler a -> Either CompilerErr [AlmostInstruction]
runCompiler = runExcept . (`evalStateT` emptyEnv) . execWriterT

compileProg :: Jlt.Prog -> Either CompilerErr LLVM.Prog
compileProg = aux . rename
  where
    aux p@(Jlt.Program defs) = do
        -- NOTE I don't think we actually need to persist the state across the
        -- different topdefs.
        pDefs <- mapM trTopDef defs
        return LLVM.Prog
          { LLVM.pGlobals = collectGlobals p
          , LLVM.pDecls   = builtinDecls
          , LLVM.pDefs    = pDefs
          }

-- | TODO Stub!
collectGlobals :: Jlt.Prog -> [LLVM.GlobalVar]
collectGlobals _ = []

trTopDef :: Jlt.TopDef -> Either CompilerErr LLVM.Def
trTopDef (Jlt.FnDef t i args blk) = do
  bss <- fmap almostToBlks $ runCompiler $ do
    entry <- newLabel
    fallThrough <- newLabel
    emitLabel entry
    trBlk fallThrough blk
    emitLabel fallThrough
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = bss
    }

trBlk
  :: MonadCompile m
  => LLVM.Label -> Jlt.Blk -> m ()
trBlk fallthrough (Jlt.Block stmts) = mapM_ (trStmt fallthrough) stmts

unreachable :: LLVM.Instruction
unreachable = LLVM.Unreachable

type AlmostInstruction = Either LLVM.Label LLVM.Instruction
almostToBlks :: [AlmostInstruction] -> [LLVM.Blk]
almostToBlks [] = []
almostToBlks (x : xs) = map (orderAllocsBlks . atLeastOne) . synth $ case x of
  Left lbl -> foldl go (lbl             , [] , []) xs
  Right i  -> foldl go (LLVM.Label "err", [i], []) xs
  where
    go
      :: (LLVM.Label, [LLVM.Instruction], [LLVM.Blk])
      -> AlmostInstruction
      -> (LLVM.Label, [LLVM.Instruction], [LLVM.Blk])
    go (lbl, prev, acc) e = case e of
      Left lbl' -> (lbl', [], acc ++ [LLVM.Blk lbl prev])
      Right i   -> (lbl, prev ++ [i], acc)
    synth (lbl, is, acc) = acc ++ [LLVM.Blk lbl is]
    atLeastOne blk@(LLVM.Blk lbl is) = case is of
      [] -> LLVM.Blk lbl [unreachable]
      _  -> blk
    orderAllocsBlks (LLVM.Blk lbl is) = (LLVM.Blk lbl (orderAllocs is))

orderAllocs :: [LLVM.Instruction] -> [LLVM.Instruction]
orderAllocs = uncurry (++) . foldl go ([],[])
  where
    go (allocs, nonallocs) i = case i of
      LLVM.Alloca{} -> (allocs ++ [i], nonallocs)
      _             -> (allocs, nonallocs ++ [i])

-- "Type synonyms cannot be partially-applied"
-- From: http://stackoverflow.com/a/9289928/1021134
-- But this is also the type:
--     Jlt.Stmt -> WriterT AlmostInstruction Compiler ()
trStmt
  :: MonadCompile m
  => LLVM.Label -> Jlt.Stmt -> m ()
trStmt fallThrough s = case s of
  Jlt.Empty -> return ()
  Jlt.BStmt b -> trBlk fallThrough b
  Jlt.Decl typ its -> mapM_ (varInit typ) its
  Jlt.Ass i e -> assign i e
  Jlt.Incr i -> incr i >>= emitInstructions
  Jlt.Decr i -> decr i >>= emitInstructions
  Jlt.Ret e -> llvmReturn e
  Jlt.VRet -> llvmVoidReturn
  Jlt.Cond e s0 -> do
    t <- newLabel
    cond e t fallThrough
    emitLabel t
    cont s0
  Jlt.CondElse e s0 s1 -> do
    t <- newLabel
    f <- newLabel
    cond e t f
    emitLabel t
    cont s0
    emitLabel f
    cont s1
  Jlt.While e s0 -> do
    lblCond <- newLabelNamed "whileCond"
    lblBody <- newLabelNamed "whileBody"
    emitLabel lblCond
    cond e lblBody fallThrough
    emitLabel lblBody
    cont s0
  Jlt.SExp e -> trExpr e
  where
    cont = trStmt fallThrough

emitInstructions :: MonadWriter [Either a s]  m => [s] -> m ()
emitInstructions = tell . map Right

emitLabel :: MonadWriter [Either a s] m => a -> m ()
emitLabel = tell . pure . Left

assign :: MonadCompile m
  => Jlt.Ident -> Jlt.Expr -> m ()
assign i e = do
  op <- resultOfExpression e
  let tp = trType (typeof e)
      reg = trNameToReg i
  emitInstructions [LLVM.Store tp op tp reg]

-- TODO: Dummy value
typeof :: Jlt.Expr -> Jlt.Type
typeof (Jlt.EAnn tp _) = tp
typeof _ = Jlt.Void

llvmReturn
  :: MonadCompile m
  => Jlt.Expr -> m ()
llvmReturn e = do
  op <- resultOfExpression e
  emitInstructions [LLVM.Return dummyTp op]

showOp :: Show a => Either t a -> String
showOp op = case op of
  Right x -> show x
  Left{} -> "?"

-- TODO
llvmVoidReturn :: MonadCompile m => m ()
llvmVoidReturn = emitInstructions [LLVM.Pseudo "void return"]

cond :: MonadCompile m
  => Jlt.Expr -> LLVM.Label -> LLVM.Label -> m ()
cond e t f = do
  op <- resultOfExpression e
  emitInstructions [LLVM.BranchCond op t f]

incr, decr :: MonadCompile m => Jlt.Ident -> m [LLVM.Instruction]
incr i = do
  let tp = LLVM.I32
      xptr = trIdent i
  xval <- newReg
  valIncr <- newReg
  return
    [ LLVM.Load tp (LLVM.Pointer tp) xptr xval
    , LLVM.Add tp (Left xval) (Right 1) valIncr
    , LLVM.Store LLVM.I64 (Left valIncr) LLVM.I64 xptr
    ]
decr = undefined

newReg :: MonadCompile m => m LLVM.Reg
newReg = return (LLVM.Reg "stub")

-- | Assumes that the item is already initialized and exists in scope.
varInit
  :: MonadCompile m
  => Jlt.Type -> Jlt.Item -> m ()
varInit jltType itm = do
  -- (reg, tp) <- undefined -- lookupItem itm >>= maybeToErr' (Generic "var init - cant find")
  let tp :: LLVM.Type
      tp = trType jltType
      reg :: LLVM.Reg
      reg = trIdent . itemName $ itm
      varInit'
        :: MonadCompile m
        => LLVM.Operand -> m ()
      varInit' op = emitInstructions
        [ LLVM.Alloca tp reg
        , LLVM.Store tp op (LLVM.Pointer tp) reg
        ]
  case itm of
    Jlt.NoInit{} -> varInit' (defaultValue jltType)
    -- Here we must first compute the value of the expression
    -- and store the result of that in the variable.
    Jlt.Init _ e -> do
      reg0 <- resultOfExpression e
      varInit' reg0

trIdent :: Jlt.Ident -> LLVM.Reg
trIdent (Jlt.Ident s) = LLVM.Reg s

defaultValue :: Jlt.Type -> LLVM.Operand
defaultValue t = case t of
  Jlt.Int -> Right 0
  _   -> Right 0

itemName :: Jlt.Item -> Jlt.Ident
itemName itm = case itm of
  Jlt.NoInit i -> i
  Jlt.Init i _  -> i

trName :: Jlt.Ident -> LLVM.Name
trName (Jlt.Ident s) = LLVM.Name s

trNameToReg :: Jlt.Ident -> LLVM.Reg
trNameToReg (Jlt.Ident s) = LLVM.Reg s

trArg :: Jlt.Arg -> LLVM.Type
trArg (Jlt.Argument t _) = trType t

trType :: Jlt.Type -> LLVM.Type
trType t = case t of
  Jlt.Int -> LLVM.I64
  -- TODO This is wrong!
  Jlt.Doub -> LLVM.I64
  Jlt.Bool -> undefined
  Jlt.Void -> LLVM.Void
  Jlt.String -> undefined
  Jlt.Fun _t _tArgs -> undefined

builtinDecls :: [LLVM.Decl]
builtinDecls =
  [ LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Name "printInt"
    , LLVM.declArgs = [LLVM.I32]
    }
  ]

-- For now we will just assume that the expression is `42`.
resultOfExpression
  :: MonadCompile m
  => Jlt.Expr -> m LLVM.Operand
resultOfExpression e = case e of
  Jlt.EVar{} -> todo
  Jlt.ELitInt x -> return $ Right (fromInteger x)
  Jlt.ELitDoub d -> return $ Right (round d)
  Jlt.ELitTrue -> return $ Right 1
  Jlt.ELitFalse -> return $ Right 0
  Jlt.EAnn _ e' -> resultOfExpression e'
  _ -> todo
  where
    todo = return $ Right 42

trExprTp
  :: MonadCompile m
  => Jlt.Type -> Jlt.Expr -> m ()
trExprTp _tp e = case e of
  Jlt.EVar{} -> pse "var"
  Jlt.ELitInt{} -> pse "lit-int"
  Jlt.ELitDoub{} -> pse "lit-doub"
  Jlt.ELitTrue -> pse "true"
  Jlt.ELitFalse{} -> pse "false"
  Jlt.EApp _i@(Jlt.Ident inm) es -> do
    ops <- mapM resultOfExpression es
    pse $ "call " ++ inm ++ intercalate "," (map showOp ops)
  Jlt.EString s -> pse ("str " ++ s)
  Jlt.Neg{} -> pse "neg"
  Jlt.Not{} -> pse "not"
  Jlt.EMul{} -> pse "mul"
  Jlt.EAdd{} -> pse "add"
  Jlt.ERel{} -> pse "rel"
  Jlt.EAnd{} -> pse "and"
  Jlt.EOr{} -> pse "or"
  Jlt.EAnn tp' e0 -> trExprTp tp' e0
  where
    pse s = emitInstructions [LLVM.Pseudo s]

trExpr
  :: MonadCompile m
  => Jlt.Expr -> m ()
trExpr e = case e of
  Jlt.EAnn tp e' -> trExprTp tp e'
  _              -> trExprTp err e
  where
    err = error "IMPOSSIBLE - Should've been annotated by the type-checker"

dummyTp :: LLVM.Type
dummyTp = LLVM.I64
