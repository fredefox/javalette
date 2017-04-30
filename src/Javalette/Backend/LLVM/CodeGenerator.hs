{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Javalette.Backend.LLVM.CodeGenerator
  ( compileProg
  , CompilerErr
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M

import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint
import Javalette.Backend.LLVM.Renamer (rename)

data CompilerErr = Generic String deriving (Show)
data Env = Env
  -- Each time a variable is encountered it is mapped to the next number in a
  -- sequence.
  { counterLabels :: Int
  , counterRegs   :: Int
  } deriving (Show)

incrCounterLabels :: MonadState Env m => m Int
incrCounterLabels = do
  lbl <- gets counterLabels
  modify (\e -> e { counterLabels = succ lbl })
  return lbl

incrCounterRegs :: MonadState Env m => m Int
incrCounterRegs = do
  lbl <- gets counterRegs
  modify (\e -> e { counterRegs = succ lbl })
  return lbl

newLabel :: MonadState Env m => m LLVM.Label
newLabel = LLVM.Label . ('l':) . show <$> incrCounterLabels

newLabelNamed :: MonadCompile m => String -> m LLVM.Label
--newLabelNamed s = LLVM.Label . (s ++ ) . show <$> incrCounterLabels
newLabelNamed _ = newLabel

newReg :: MonadCompile m => m LLVM.Reg
newReg = LLVM.Reg . ('t':) . show <$> incrCounterRegs

-- TODO We should traverse the ast, collect all strings, put them in a map
-- and then declare them at the top of the llvm output. We should then lookup
-- the name of that declaration and use the pointer to that value whenever a
-- given string is needed.
lookupString :: MonadCompile m => String -> m LLVM.Name
lookupString s = do
  m <- asks envConstants
  case M.lookup s m of
    Nothing -> throwError (Generic "Could not find string constant")
    Just x -> return x

emptyEnv :: Env
emptyEnv = Env 0 0

instance Pretty CompilerErr where
  pPrint err = case err of
    Generic s -> text "ERR:" <+> text s

type Compiler a
  = ReaderT ReadEnv
    ( WriterT [AlmostInstruction]
      ( StateT Env (Except CompilerErr)
      )
    ) a

data ReadEnv = ReadEnv
  { envConstants :: Constants
  , envFunctions :: Functions
  }

type Constants = Map String LLVM.Name
-- Maps functions to their argument types.
type Functions = Map String [LLVM.Type]

lookupFunction :: MonadCompile m => LLVM.Name -> m [LLVM.Type]
lookupFunction (LLVM.Name n) = do
  m <- asks envFunctions
  case M.lookup n m of
    Nothing -> throwError (Generic $ "Could not find function" ++ show n)
    Just f -> return f

getArgTypes :: MonadCompile m => LLVM.Name -> m [LLVM.Type]
getArgTypes = lookupFunction

type MonadCompile m =
  ( MonadWriter [AlmostInstruction] m
  , MonadState Env m
  , MonadError CompilerErr m
  , MonadReader ReadEnv m
  )

runCompiler :: ReadEnv -> Compiler a -> Either CompilerErr [AlmostInstruction]
runCompiler re = runExcept . (`evalStateT` emptyEnv) . execWriterT . (`runReaderT` re)

compileProg :: Jlt.Prog -> Either CompilerErr LLVM.Prog
compileProg = aux . rename
  where
    aux (Jlt.Program defs) = do
        let cs = constantsMap $ concatMap collectStringsTopDef defs
            declTypes = map (\d -> (unname (LLVM.declName d), LLVM.declArgs d)) builtinDecls
            defTypes  = map (\d -> (unname (trTopDefName d), trArgType d)) defs
            re = ReadEnv cs (M.fromList (declTypes ++ defTypes))
        pDefs <- mapM (trTopDef re) defs
        return LLVM.Prog
          { LLVM.pGlobals = map declString (M.toList cs)
          , LLVM.pDecls   = builtinDecls
          , LLVM.pDefs    = pDefs
          }
    constantsMap :: [String] -> Constants
    constantsMap xs = M.fromList . map (fmap intToReg) $ zip xs [0..]
    intToReg :: Int -> LLVM.Name
    intToReg = LLVM.Name . (:) 's' . show
    declString :: (String, LLVM.Name) -> LLVM.GlobalVar
    declString (s, r) = LLVM.GlobalVar
      { LLVM.gvName = r , LLVM.gvType = stringType s , LLVM.gvVal = LLVM.Constant s }
    trArgType (Jlt.FnDef _ _ args _) = map argType args
    trTopDefName (Jlt.FnDef _ i _ _) = trName i
    unname :: LLVM.Name -> String
    unname (LLVM.Name s) = s

collectStringsTopDef :: Jlt.TopDef -> [String]
collectStringsTopDef (Jlt.FnDef _ _ _ b) = collectStringsBlk b

collectStringsBlk :: Jlt.Blk -> [String]
collectStringsBlk (Jlt.Block stmts) = concatMap collectStringsStmt stmts

collectStringsStmt :: Jlt.Stmt -> [String]
collectStringsStmt s = case s of
  Jlt.Empty -> []
  Jlt.BStmt b -> collectStringsBlk b
  Jlt.Decl _ its -> concatMap collectStringsItem its
  Jlt.Ass _ e -> collectStringsExpr e
  Jlt.Incr{} -> impossibleRemoved ; Jlt.Decr{} -> impossibleRemoved
  Jlt.Ret e -> collectStringsExpr e
  Jlt.VRet -> []
  Jlt.Cond e s0 -> collectStringsExpr e ++ collectStringsStmt s0
  Jlt.CondElse e s0 s1 -> concat
    [ collectStringsExpr e
    , collectStringsStmt s0
    , collectStringsStmt s1
    ]
  Jlt.While e s0 -> collectStringsExpr e ++ collectStringsStmt s0
  Jlt.SExp e -> collectStringsExpr e

collectStringsItem :: Jlt.Item -> [String]
collectStringsItem i = case i of
  Jlt.NoInit{} -> []
  Jlt.Init _ e -> collectStringsExpr e

collectStringsExpr :: Jlt.Expr -> [String]
collectStringsExpr e = case e of
  Jlt.EVar{} -> [] ; Jlt.ELitInt{} -> []; Jlt.ELitDoub{} -> []
  Jlt.ELitTrue -> []; Jlt.ELitFalse -> []; Jlt.EApp _ es -> concatMap collectStringsExpr es
  Jlt.EString s -> pure s; Jlt.Neg e0 -> collectStringsExpr e0
  Jlt.Not e0 -> collectStringsExpr e0;
  Jlt.EMul e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAdd e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.ERel e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAnd e0 e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EOr e0 e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAnn _ e0 -> collectStringsExpr e0

impossibleRemoved :: a
impossibleRemoved = error "IMPOSSIBLE - removed by typechecker"

trTopDef :: ReadEnv -> Jlt.TopDef -> Either CompilerErr LLVM.Def
trTopDef re (Jlt.FnDef t i args blk) = do
  bss <- runCompiler re $ do
    entry <- newLabel
    fallThrough <- newLabel
    emitLabel entry
    mapM_ cgArg args
    trBlk fallThrough blk
    emitLabel fallThrough
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = almostToBlks bss
    }

trArg :: Jlt.Arg -> LLVM.Arg
trArg (Jlt.Argument t i) = LLVM.Arg (trType t) (trNameToRegArg i)

cgArg :: MonadCompile m => Jlt.Arg -> m ()
cgArg (Jlt.Argument t i) = varInit (trType t) (trNameToReg i) (Left (trNameToRegArg i))

trBlk
  :: MonadCompile m
  => LLVM.Label -> Jlt.Blk -> m ()
trBlk fallthrough (Jlt.Block stmts) = mapM_ (trStmt fallthrough) stmts

unreachable :: LLVM.TermInstr
unreachable = LLVM.Unreachable

data AlmostInstruction
  = Label LLVM.Label
  | Instr LLVM.Instruction
  | TermInstr LLVM.TermInstr
  deriving (Show)

almostToBlks :: [AlmostInstruction] -> [LLVM.Blk]
almostToBlks [] = []
almostToBlks (x : xs) = map orderAllocsBlks . synth $ case x of
  Label lbl    -> foldl go (lbl, [], unreachable, []) xs
  Instr{}      -> undefined
  TermInstr{}  -> undefined
  where
    go
      :: (LLVM.Label, [LLVM.Instruction], LLVM.TermInstr, [LLVM.Blk])
      -> AlmostInstruction
      -> (LLVM.Label, [LLVM.Instruction], LLVM.TermInstr, [LLVM.Blk])
    go tt@(lbl, prev, ti, acc) curr = case curr of
      Label lbl'    -> (lbl', []         , unreachable, acc ++ [LLVM.Blk lbl prev ti])
      Instr i       -> (lbl , prev ++ [i], ti         , acc)
      TermInstr ti' -> (lbl , prev       , ti'        , acc)
    synth (lbl, is, ti, acc) = acc ++ [LLVM.Blk lbl is ti]
    -- NOTE This is actually not needed
    orderAllocsBlks (LLVM.Blk lbl is ti) = LLVM.Blk lbl (orderAllocs is) ti

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
  Jlt.Decl typ its -> mapM_ (itemInit typ) its
  Jlt.Ass i e -> assign i e
  Jlt.Incr{} -> undefined "IMPOSSIBLE - removed by typechecker"
  Jlt.Decr{} -> undefined "IMPOSSIBLE - removed by typechecker"
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
    jumpToNew lblCond
    cond e lblBody fallThrough
    emitLabel lblBody
    cont s0
  Jlt.SExp e -> trExpr e
  where
    cont = trStmt fallThrough

emitInstructions :: MonadWriter [AlmostInstruction]  m => [LLVM.Instruction] -> m ()
emitInstructions = tell . map Instr

jumpToNew :: MonadWriter [AlmostInstruction] m => LLVM.Label -> m ()
jumpToNew lbl = do
  emitTerminator (LLVM.Branch lbl)
  emitLabel lbl

emitLabel :: MonadWriter [AlmostInstruction] m => LLVM.Label -> m ()
emitLabel = tell . pure . Label

emitTerminator :: MonadWriter [AlmostInstruction] m => LLVM.TermInstr -> m ()
emitTerminator = tell . pure . TermInstr

assign :: MonadCompile m
  => Jlt.Ident -> Jlt.Expr -> m ()
assign i e = do
  op <- resultOfExpression e
  let tp = trType (typeof e)
      reg = trNameToReg i
  emitInstructions [LLVM.Store tp op (LLVM.Pointer tp) reg]

typeof :: Jlt.Expr -> Jlt.Type
typeof (Jlt.EAnn tp _) = tp
typeof _ = error "IMPOSSIBLE - should've been removed by the typechecker"

llvmReturn
  :: MonadCompile m
  => Jlt.Expr -> m ()
llvmReturn e = do
  op <- resultOfExpression e
  let tp = trType (typeof e)
  emitTerminator (LLVM.Return tp op)

showOp :: Show a => Either t a -> String
showOp op = case op of
  Right x -> show x
  Left{} -> "?"

llvmVoidReturn :: MonadCompile m => m ()
llvmVoidReturn = emitTerminator LLVM.VoidReturn

cond :: MonadCompile m
  => Jlt.Expr -> LLVM.Label -> LLVM.Label -> m ()
cond e t f = do
  op <- resultOfExpression e
  emitTerminator (LLVM.BranchCond op t f)

-- | Assumes that the item is already initialized and exists in scope.
itemInit
  :: MonadCompile m
  => Jlt.Type -> Jlt.Item -> m ()
itemInit jltType itm = do
  -- (reg, tp) <- undefined -- lookupItem itm >>= maybeToErr' (Generic "var init - cant find")
  let tp :: LLVM.Type
      tp = trType jltType
      reg :: LLVM.Reg
      reg = trIdent . itemName $ itm
  case itm of
    Jlt.NoInit{} -> varInit tp reg (defaultValue jltType)
    -- Here we must first compute the value of the expression
    -- and store the result of that in the variable.
    Jlt.Init _ e -> do
      reg0 <- resultOfExpression e
      varInit tp reg reg0


varInit
  :: MonadCompile m
  => LLVM.Type -> LLVM.Reg -> LLVM.Operand -> m ()
varInit tp reg op = emitInstructions
  [ LLVM.Alloca tp reg
  , LLVM.Store tp op (LLVM.Pointer tp) reg
  ]
trIdent :: Jlt.Ident -> LLVM.Reg
trIdent (Jlt.Ident s) = LLVM.Reg s

defaultValue :: Jlt.Type -> LLVM.Operand
defaultValue t = case t of
  Jlt.Int -> Right (LLVM.ValInt 0)
  Jlt.Doub -> Right (LLVM.ValDoub 0)
  _   -> error $ "What's the default value of " ++ show t

itemName :: Jlt.Item -> Jlt.Ident
itemName itm = case itm of
  Jlt.NoInit i -> i
  Jlt.Init i _  -> i

trName :: Jlt.Ident -> LLVM.Name
trName (Jlt.Ident s) = LLVM.Name s

trNameToReg :: Jlt.Ident -> LLVM.Reg
trNameToReg (Jlt.Ident s) = LLVM.Reg s

trNameToRegArg :: Jlt.Ident -> LLVM.Reg
trNameToRegArg (Jlt.Ident s) = LLVM.Reg (s ++ ".val")

argType :: Jlt.Arg -> LLVM.Type
argType (Jlt.Argument t _) = trType t

trType :: Jlt.Type -> LLVM.Type
trType t = case t of
  Jlt.Int -> LLVM.I 32
  Jlt.Doub -> LLVM.Double
  Jlt.Bool -> LLVM.I 1
  Jlt.Void -> LLVM.Void
  Jlt.String -> error
    $  "The string type cannot be translated directly. "
    ++ "Strings of different length have different types"
  Jlt.Fun _t _tArgs -> undefined

builtinDecls :: [LLVM.Decl]
builtinDecls =
  [ LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Name "printInt"
    , LLVM.declArgs = [LLVM.I 32]
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Name "printDouble"
    , LLVM.declArgs = [LLVM.Double]
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Name "printString"
    , LLVM.declArgs = [LLVM.Pointer (LLVM.I 8)]
    }
  ]

resultOfExpressionTp
  :: MonadCompile m
  => Jlt.Type -> Jlt.Expr -> m LLVM.Operand
resultOfExpressionTp tp e = case e of
  Jlt.EVar i -> do
    r <- newReg
    let tp' = trType tp
    emitInstructions [LLVM.Load tp' (LLVM.Pointer tp') (trNameToReg i) r]
    return (Left r)
  Jlt.ELitInt x -> return $ Right (LLVM.ValInt $ fromInteger x)
  Jlt.ELitDoub d -> return $ Right (LLVM.ValDoub d)
  Jlt.ELitTrue -> return $ Right (LLVM.ValInt 0)
  Jlt.ELitFalse -> return $ Right (LLVM.ValInt 1)
  Jlt.EAnn tp' e' -> resultOfExpressionTp tp' e'
  Jlt.EApp i es -> do
    es' <- es `forM` \(Jlt.EAnn tp' e') -> do
      r' <- resultOfExpressionTp tp' e'
      return (trType tp', r')
    r <- newReg
    emitInstructions [LLVM.Call (trType tp) (trName i) es' r]
    return (Left r)
  Jlt.ERel e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    let tp' = trType $ typeof e0
        i   = case tp' of
          LLVM.I{} -> LLVM.Icmp
          LLVM.Double -> LLVM.Fcmp
    emitInstructions [i (relOp op tp') tp' r0 r1 r]
    return (Left r)
  Jlt.EMul e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    emitInstructions [mulOp op (trType tp) r0 r1 r]
    return (Left r)
  Jlt.EAdd e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    emitInstructions [addOp op (trType tp) r0 r1 r]
    return (Left r)
  Jlt.EAnd e0 e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    emitInstructions [LLVM.And (trType tp) r0 r1 r]
    return (Left r)
  Jlt.EOr e0 e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    emitInstructions [LLVM.Or (trType tp) r0 r1 r]
    return (Left r)
  Jlt.Neg e0 -> do
    r0 <- resultOfExpression e0
    r <- newReg
    let tp' = trType tp
    emitInstructions [LLVM.Sub tp' (zero tp') r0 r]
    return (Left r)
  Jlt.Not e0 -> do
    r0 <- resultOfExpression e0
    r <- newReg
    emitInstructions [LLVM.Pseudo $ "not " ++ show r0]
    return (Left r)
  Jlt.EString s -> do
    sReg <- lookupString s
    r <- newReg
    let tp' = stringType s
        path = [(LLVM.I 32, 0), (LLVM.I 32, 0)]
    emitInstructions [LLVM.GetElementPtr tp' (LLVM.Pointer tp') sReg path r]
    return (Left r)

zero :: LLVM.Type -> LLVM.Operand
zero t = case t of
  LLVM.I{} -> Right (LLVM.ValInt 0)
  LLVM.Double -> Right (LLVM.ValDoub 0)

stringType :: String -> LLVM.Type
stringType s = LLVM.Array (length s) (LLVM.I 8)

mulOp
  :: Jlt.MulOp
     -> LLVM.Type
     -> LLVM.Operand
     -> LLVM.Operand
     -> LLVM.Reg
     -> LLVM.Instruction
mulOp op tp = case tp of
  LLVM.I{} -> case op of
    Jlt.Times -> LLVM.Mul tp
    Jlt.Div   -> LLVM.SDiv tp
    Jlt.Mod   -> LLVM.Rem tp
  LLVM.Double -> case op of
    Jlt.Times -> LLVM.FMul tp
    Jlt.Div   -> LLVM.FDiv tp

relOp
  :: Jlt.RelOp
  -> LLVM.Type
  -> LLVM.Comparison
relOp op tp = case op of
  Jlt.LTH -> LLVM.SLT
  Jlt.LE  -> LLVM.SLE
  Jlt.GTH -> LLVM.SGT
  Jlt.GE  -> LLVM.SGE
  Jlt.EQU -> case tp of LLVM.I{} -> LLVM.EQ ; LLVM.Double -> LLVM.OEQ
  Jlt.NE  -> LLVM.NE

addOp
  :: Jlt.AddOp
     -> LLVM.Type
     -> LLVM.Operand
     -> LLVM.Operand
     -> LLVM.Reg
     -> LLVM.Instruction
addOp op tp = case tp of
  LLVM.I{} -> case op of
    Jlt.Plus  -> LLVM.Add tp
    Jlt.Minus -> LLVM.Sub tp
  LLVM.Double -> case op of
    Jlt.Plus  -> LLVM.FAdd tp
    Jlt.Minus -> LLVM.FSub tp

resultOfExpression :: MonadCompile m
  => Jlt.Expr -> m LLVM.Operand
resultOfExpression e = case e of
  Jlt.EAnn tp e' -> resultOfExpressionTp tp e'
  _         -> error "IMPOSSIBLE - was removed during type-checking"

trExprTp
  :: MonadCompile m
  => Jlt.Type -> Jlt.Expr -> m ()
trExprTp tp e = case e of
  Jlt.EVar{} -> pse "var"
  Jlt.ELitInt{} -> pse "lit-int"
  Jlt.ELitDoub{} -> pse "lit-doub"
  Jlt.ELitTrue -> pse "true"
  Jlt.ELitFalse{} -> pse "false"
  Jlt.EApp i es -> do
    ops <- mapM resultOfExpression es
    call (trType tp) (trName i) ops
  Jlt.EString s -> pse ("str " ++ s)
  Jlt.Neg{} -> pse "neg"
  Jlt.Not{} -> pse "not"
  Jlt.EMul{} -> pse "mul"
  Jlt.EAdd{} -> pse "add"
  Jlt.ERel{} -> pse "rel"
  Jlt.EAnd{} -> pse "and"
  Jlt.EOr{} -> pse "or"
  -- This ought not to occur:
  Jlt.EAnn tp' e0 -> trExprTp tp' e0
  where
    pse s = emitInstructions [LLVM.Pseudo s]

-- Call Type Name [(Type, Operand)] Reg
call :: MonadCompile m => LLVM.Type -> LLVM.Name -> [LLVM.Operand] -> m ()
call t n ops = do
  opTypes <- getArgTypes n
  let tps = zip opTypes ops
  case t of
    LLVM.Void -> emitInstructions [ LLVM.CallVoid t n tps ]
    _ -> do
      r <- newReg
      emitInstructions [ LLVM.Call t n (zip opTypes ops) r ]

trExpr
  :: MonadCompile m
  => Jlt.Expr -> m ()
trExpr e = case e of
  Jlt.EAnn tp e' -> trExprTp tp e'
  _              -> err
  where
    err = error "IMPOSSIBLE - Should've been annotated by the type-checker"

dummyTp :: LLVM.Type
dummyTp = LLVM.I 64
