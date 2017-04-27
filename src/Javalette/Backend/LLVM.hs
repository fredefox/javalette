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

import Javalette.Backend.Internals
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint
import Javalette.Backend.LLVM.Renamer (rename)

import Javalette.Syntax (sampleProg, smallProg)
import Debug.Trace

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

-------------------
-- Closure stuff --
-------------------

type Scope = [Map Jlt.Ident Jlt.Ident]

emptyScope :: Scope
emptyScope = []

-- | Assigns a name to `a` in the current closure and returns it. Do not call
-- `newVar` on a variable that has already been assigned or on a scope with no
-- closures.
mapVar :: Jlt.Ident -> Jlt.Type -> Compiler Jlt.Ident
mapVar a t = do
  vars <- getVariables
  (c:cs) <- case vars of
    [] -> throwError (Generic "Cannot map variables with no closures at hand")
    _ -> return vars
  i <- fresh
  let b = intToIdent i
  putVariables (M.insert a b c : cs)
  return b
  where
    intToIdent = Jlt.Ident . ('v':) . show

fresh :: Compiler Int
fresh = undefined

putVariables :: Variables -> Compiler ()
putVariables v = modify (\e -> e { variables = v })

newVar :: LLVM.Type -> Compiler Jlt.Ident
newVar t = do
  vars <- getVariables
  case vars of
    [] -> error "Cannot create variables with no closures at hand"
    (c:cs) -> undefined

lookupFirst :: Ord a => a -> [Map a b] -> Maybe b
lookupFirst _ [] = Nothing
lookupFirst a (x : xs) = case M.lookup a x of
  Nothing -> lookupFirst a xs
  Just b  -> return b

type Variables = Scope

data CompilerErr = Generic String deriving (Show)
data Env = Env
  -- Each time a variable is encountered it is mapped to the next number in a
  -- sequence.
  { variables :: Variables
  , maxLabel :: Int
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

getVariables :: Compiler Variables
getVariables = gets variables

lookupIdent :: Jlt.Ident -> Compiler (Maybe Jlt.Ident)
lookupIdent i = do
  vars <- getVariables
  return $ lookupFirst i vars

intToName :: Int -> LLVM.Reg
intToName = LLVM.Reg . ('v':) . show

emptyEnv :: Env
emptyEnv = Env emptyScope 0

instance Pretty CompilerErr where
  pPrint err = case err of
    Generic s -> text "ERR:" <+> text s

type Compiler a = StateT Env (Except CompilerErr) a

type MonadCompile m = (MonadState Env m, MonadError CompilerErr m)

runCompiler :: Compiler a -> Either CompilerErr a
runCompiler = runExcept . (`evalStateT` emptyEnv)

runCompiler' = runExcept . (`runStateT` emptyEnv)

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

{-
controlBlk :: Jlt.Blk -> [[Jlt.Stmt]]
controlBlk (Jlt.Block stmts) = foldl go [] stmts
  where
    go acc s = case s of
      Jlt.Empty -> acc
      Jlt.BStmt b -> acc ++ controlBlk b
      Jlt.Decl{} -> acc ++ [s]
      Jlt.Ass{} -> acc ++ [s]
      Jlt.Incr{} -> acc ++ [s]
      Jlt.Decr{} -> acc ++ [s]
      Jlt.Ret{} -> acc ++ [s]
      Jlt.VRet -> acc ++ [s]
      Jlt.Cond e s0 -> undefined

controlStmt :: Jlt.Stmt -> [Jlt.Stmt]
controlStmt = undefined
-}

-- | TODO Stub!
collectGlobals :: Jlt.Prog -> [LLVM.GlobalVar]
collectGlobals _ = []

trTopDef :: Jlt.TopDef -> Either CompilerErr LLVM.Def
trTopDef (Jlt.FnDef t i args blk) = do
  bss <- runCompiler $ fmap almostToBlks $ execWriterT $ do
    entry <- newLabel
    fallThrough <- newLabel
    emitLabel entry
    trBlkAlt fallThrough blk
    emitLabel fallThrough
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = bss
    }

trBlk :: Jlt.Blk -> Compiler [LLVM.Blk]
trBlk bs = do
  lbl <- newLabel
  trBlkWithLabel lbl [] bs

trBlkAlt
  :: (MonadWriter [AlmostInstruction] m, MonadCompile m)
  => LLVM.Label -> Jlt.Blk -> m ()
trBlkAlt fallthrough (Jlt.Block stmts) = mapM_ (trStmtAlt fallthrough) stmts

trBlkWithLabel :: LLVM.Label -> [LLVM.Blk] -> Jlt.Blk -> Compiler [LLVM.Blk]
trBlkWithLabel lbl prev (Jlt.Block stmts) = do
--   blks <- foldM (trStmt lbl) prev stmts
  blks <- almostToBlks <$> execWriterT (mapM (trStmtAlt lbl) stmts)
  b <- unreachable lbl
  return (blks ++ [b])

unreachable :: LLVM.Label -> Compiler LLVM.Blk
unreachable lbl = return (LLVM.Blk lbl [instrUnreach])

instrUnreach :: LLVM.Instruction
instrUnreach = LLVM.Pseudo "unreachable"

withNewLabel, labelWithReturn :: Compiler [LLVM.Instruction] -> Compiler LLVM.Blk
withNewLabel act = LLVM.Blk <$> newLabel <*> act
labelWithReturn = undefined

tracePrettyId x = trace (prettyShow x) x
tracePretty x = trace (prettyShow x)

type AlmostInstruction = Either LLVM.Label LLVM.Instruction
almostToBlks :: [AlmostInstruction] -> [LLVM.Blk]
almostToBlks [] = []
almostToBlks (x : xs) = synth $ case x of
  Left lbl -> foldl go (lbl               , [] , []) xs
  Right i  -> foldl go (LLVM.Label "dummy", [i], []) xs
  where
    go
      :: (LLVM.Label, [LLVM.Instruction], [LLVM.Blk])
      -> AlmostInstruction
      -> (LLVM.Label, [LLVM.Instruction], [LLVM.Blk])
    go (lbl, prev, acc) e = case e of
      Left lbl' -> (lbl', [], acc ++ [LLVM.Blk lbl prev])
      Right i   -> (lbl, prev ++ [i], acc)
    synth (lbl, is, acc) = acc ++ [LLVM.Blk lbl is]

-- "Type synonyms cannot be partially-applied"
-- From: http://stackoverflow.com/a/9289928/1021134
-- But this is also the type:
--     Jlt.Stmt -> WriterT AlmostInstruction Compiler ()
trStmtAlt
  :: (MonadWriter [AlmostInstruction] m, MonadCompile m)
  => LLVM.Label -> Jlt.Stmt -> m ()
trStmtAlt fallThrough s = case s of
  Jlt.Empty -> return ()
  Jlt.BStmt b -> trBlkAlt fallThrough b
  Jlt.Decl typ its -> mapM_ (varInitAlt typ) its
  Jlt.Ass i e -> assign i e >>= emitInstructions
  Jlt.Incr i -> incr i >>= emitInstructions
  Jlt.Decr i -> decr i >>= emitInstructions
  Jlt.Ret e -> llvmReturn e >>= emitInstructions
  Jlt.VRet -> llvmVoidReturn >>= emitInstructions
  Jlt.Cond e s0 -> do
    t <- newLabel
    condAlt e t fallThrough
    emitLabel t
    cont s0
  Jlt.CondElse e s0 s1 -> do
    t <- newLabel
    f <- newLabel
    condAlt e t f
    emitLabel t
    cont s0
    emitLabel f
    cont s1
  Jlt.While e s0 -> do
    lblCond <- newLabelNamed "whileCond"
    lblBody <- newLabelNamed "whileBody"
    emitLabel lblCond
    condAlt e lblBody fallThrough
    traceShow s0 `seq` emitLabel lblBody
    traceShow s0 `seq` cont (traceShowId lblBody `seq` s0)
  Jlt.SExp e -> trExpr e >>= emitInstructions
  where
    cont = trStmtAlt fallThrough

emitInstructions :: MonadWriter [Either a s]  m => [s] -> m ()
emitInstructions = tell . map Right

emitLabel :: MonadWriter [Either a s] m => a -> m ()
emitLabel = tell . pure . Left

trStmt :: LLVM.Label -> [LLVM.Blk] -> Jlt.Stmt -> Compiler [LLVM.Blk]
trStmt fallThrough (tp "prev" -> prev) (traceShowId -> s) = case s of
  Jlt.Empty -> returnPrev
  Jlt.BStmt b -> newLabel >>= \l -> trBlkWithLabel l prev b >>= mergeWithPrev
  -- We should have already taken care of allocating all variables (and
  -- therefore also created a mapping for them)
  Jlt.Decl typ its -> (concat <$> mapM (varInit typ) its) >>= addToPrev
  Jlt.Ass i e -> assign i e >>= addToPrev
  Jlt.Incr i -> incr i >>= addToPrev
  Jlt.Decr i -> decr i >>= addToPrev
  Jlt.Ret e -> llvmReturn e >>= addToPrev
  Jlt.VRet -> llvmVoidReturn >>= addToPrev
  Jlt.Cond e s0 -> do
    t <- newLabel
    is <- cond e t fallThrough
    bs0 <- addToPrev is
    trStmt fallThrough (prev ++ [LLVM.Blk t []]) s0
  Jlt.CondElse e s0 s1 -> do
    t <- newLabel
    f <- newLabel
    is <- cond e t f
    bs0 <- addToPrev is
    bs1 <- trStmt fallThrough (bs0++ [LLVM.Blk t []]) s0
    trStmt fallThrough (bs1  ++ [LLVM.Blk f []]) s1
  Jlt.While e s0 -> do
    lblCond <- newLabelNamed "whileCond"
    lblBody <- newLabelNamed "whileBody"
    is <- cond e lblBody fallThrough
    let blkCond = LLVM.Blk lblCond is
    blksBody <- trStmt lblCond (prev ++ [LLVM.Blk lblBody []]) s0
    appendToPrev (blkCond : blksBody)
  Jlt.SExp e -> trExpr e >>= addToPrev
  where
    addToPrev :: [LLVM.Instruction] -> Compiler [LLVM.Blk]
    addToPrev is = case splitLast prev of
      Nothing -> do
        lbl <- newLabel
        return [LLVM.Blk lbl is]
      Just (xs, LLVM.Blk lbl is0) -> return (xs ++ [LLVM.Blk lbl (is0 ++ is)])
    mergeWithPrev :: [LLVM.Blk] -> Compiler [LLVM.Blk]
    mergeWithPrev (tp "merge" -> blks) = case splitLast prev of
      Nothing -> return blks
      Just (blks', LLVM.Blk lbl is) -> case blks of
        [] -> return prev
        (LLVM.Blk _lbl' is' : xs) -> return (blks' ++ [LLVM.Blk lbl (is ++ is')] ++ xs)
    appendToPrev :: [LLVM.Blk] -> Compiler [LLVM.Blk]
    appendToPrev bs = case splitLast prev of
      Nothing -> return bs
      Just (xs, LLVM.Blk lbl is) -> case bs of
        [] -> return prev
        ys@(LLVM.Blk (LLVM.Label lblNext) _:_) -> return
          $ xs ++ [LLVM.Blk lbl (is ++ [LLVM.Pseudo ("jmp " ++ lblNext)])] ++ ys
    returnPrev :: Compiler [LLVM.Blk]
    returnPrev = return prev

tp ss x = trace (ss ++ ":\n" ++ prettyShow x) x

-- Warning, inefficient.
splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs) = do
  (xs', x') <- splitLast xs
  return (x : xs', x')

assign :: MonadCompile m => Jlt.Ident -> Jlt.Expr -> m [LLVM.Instruction]
assign (Jlt.Ident s) e = do
  (is, reg) <- resultOfExpression e
  return $ is ++ [LLVM.Pseudo ("store " ++ prettyShow reg ++ " into " ++ s)]

llvmReturn :: MonadCompile m => Jlt.Expr -> m [LLVM.Instruction]
llvmReturn _ = return [LLVM.Pseudo "return"]

llvmVoidReturn :: MonadCompile m => m [LLVM.Instruction]
llvmVoidReturn = undefined

condAlt
  :: (MonadWriter [AlmostInstruction] m, MonadCompile m)
  => Jlt.Expr -> LLVM.Label -> LLVM.Label -> m ()
condAlt e t f = cond e t f >>= emitInstructions

cond :: MonadCompile m => Jlt.Expr -> LLVM.Label -> LLVM.Label -> m [LLVM.Instruction]
cond e (LLVM.Label t) (LLVM.Label f) = do
  (is, op) <- resultOfExpression e
  return $ is ++ [LLVM.Pseudo $ "if " ++ prettyShow op ++ " then " ++ t ++ " else " ++ f]

maybeToErr :: MonadError e m => e -> Maybe a -> m a
maybeToErr err Nothing  = throwError err
maybeToErr _   (Just x) = return x

maybeToErr' :: CompilerErr -> Maybe a -> Compiler a
maybeToErr' = maybeToErr

incr, decr :: MonadCompile m => Jlt.Ident -> m [LLVM.Instruction]
incr i = do
  let tp = LLVM.I32
      xptr = trIdent i
  xval <- newReg
  valIncr <- newReg
  return
    [ LLVM.Load tp (LLVM.Pointer tp) xptr xval
    , LLVM.Add tp (Left xval) (Right 1) valIncr
    , LLVM.Store (LLVM.I64) (Left valIncr) LLVM.I64 xptr
    ]
decr = undefined

newReg :: MonadCompile m => m LLVM.Reg
newReg = return (LLVM.Reg "stub")

pushScope, popScope :: Compiler ()
pushScope = modify (\e -> e { variables = push . variables $ e })
  where
    push :: Variables -> Variables
    push = ((:) mempty)

popScope = modify (\e -> e { variables = pop . variables $ e })
  where
    pop :: Variables -> Variables
    pop [] = error "Cannot pop on empty environment"
    pop (_:xs) = xs

withNewScope :: Compiler a -> Compiler a
withNewScope act = do
  pushScope
  a <- act
  popScope
  return a

varsBlk :: Jlt.Blk -> Compiler [LLVM.Instruction]
varsBlk (Jlt.Block stmts) = withNewScope (concat <$> mapM varsStmt stmts)

varsStmt :: Jlt.Stmt -> Compiler [LLVM.Instruction]
varsStmt s = case s of
  Jlt.Empty -> return []
  Jlt.BStmt b -> varsBlk b
  Jlt.Decl t its -> mapM (varDecl t) its
  Jlt.Ass{} -> return []
  Jlt.Incr{} -> return []
  Jlt.Decr{} -> return []
  Jlt.Ret{} -> return []
  Jlt.VRet -> return []
  Jlt.Cond _ s0 -> varsStmt s0
  Jlt.CondElse _ s0 s1 -> liftM2 (++) (varsStmt s0) (varsStmt s1)
  Jlt.While _ s0 -> varsStmt s0
  Jlt.SExp{} -> return []

varDecl :: Jlt.Type -> Jlt.Item -> Compiler LLVM.Instruction
varDecl t itm = do
  (reg, tp) <- undefined -- assignNameItem itm t
  return (LLVM.Alloca tp reg)

varInitAlt
  :: (MonadCompile m, MonadWriter [AlmostInstruction] m)
  => Jlt.Type -> Jlt.Item -> m ()
varInitAlt typ itm = varInit typ itm >>= tell . map Right

-- | Assumes that the item is already initialized and exists in scope.
varInit :: MonadCompile m => Jlt.Type -> Jlt.Item -> m [LLVM.Instruction]
varInit jltType itm = do
  -- (reg, tp) <- undefined -- lookupItem itm >>= maybeToErr' (Generic "var init - cant find")
  let tp :: LLVM.Type
      tp = trType jltType
      reg :: LLVM.Reg
      reg = trIdent . itemName $ itm
      varInit' :: MonadCompile m => LLVM.Operand -> m [LLVM.Instruction]
      varInit' op = return . pure $ LLVM.Store tp op (LLVM.Pointer tp) reg
  case itm of
    Jlt.NoInit{} -> varInit' (defaultValue jltType)
    -- Here we must first compute the value of the expression
    -- and store the result of that in the variable.
    Jlt.Init _ e -> do
      (is0, reg0) <- resultOfExpression e
      is1 <- varInit' reg0
      return (is0 ++ is1)

-- For now we will just assume that the expression is `42`.
resultOfExpression :: MonadCompile m => Jlt.Expr -> m ([LLVM.Instruction], LLVM.Operand)
resultOfExpression e = case e of
  Jlt.ELitInt x -> return ([], Right (fromInteger x))
  _ -> return ([], Right 42)

trIdent :: Jlt.Ident -> LLVM.Reg
trIdent (Jlt.Ident s) = LLVM.Reg s

defaultValue :: Jlt.Type -> LLVM.Operand
defaultValue t = case t of
  Jlt.Int -> Right 0
  _   -> Right 0

assignNameItem :: Jlt.Item -> Jlt.Type -> Compiler Jlt.Ident
assignNameItem itm t = mapVar (itemName itm) t


lookupItem :: Jlt.Item -> Compiler (Maybe Jlt.Ident)
lookupItem itm = lookupIdent (itemName itm)

itemName :: Jlt.Item -> Jlt.Ident
itemName itm = case itm of
  Jlt.NoInit i -> i
  Jlt.Init i _  -> i

trName :: Jlt.Ident -> LLVM.Name
trName (Jlt.Ident s) = LLVM.Name s

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

trExpr :: MonadCompile m => Jlt.Expr -> m [LLVM.Instruction]
trExpr e = case e of
  Jlt.EVar{} -> pse "var"
  Jlt.ELitInt{} -> pse "lit-int"
  Jlt.ELitDoub{} -> pse "lit-doub"
  Jlt.ELitTrue -> pse "true"
  Jlt.ELitFalse{} -> pse "false"
  Jlt.EApp{} -> pse "app"
  Jlt.EString s -> pse ("str " ++ s)
  Jlt.Neg{} -> pse "neg"
  Jlt.Not{} -> pse "not"
  Jlt.EMul{} -> pse "mul"
  Jlt.EAdd{} -> pse "add"
  Jlt.ERel{} -> pse "rel"
  Jlt.EAnd{} -> pse "and"
  Jlt.EOr{} -> pse "or"
  Jlt.EAnn _ e0 -> trExpr e0
  where
    pse s = return [LLVM.Pseudo s]

renamedProg = rename sampleProg

-- (Jlt.Program [mainFun]) = rename smallProg
(Jlt.Program (mainFun:_)) = renamedProg

(Jlt.FnDef _ _ _ (Jlt.Block mainStmts)) = mainFun

t = prettyPrint is
  where
    Right is = trTopDef mainFun
