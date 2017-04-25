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

backend :: Backend
backend = Backend
  { compiler = compile
  }

compile :: FilePath -> Jlt.Prog -> IO ()
compile fp = ioStuff . runCompiler . compileProg
  where
    ioStuff :: Either CompilerErr LLVM.Prog -> IO ()
    ioStuff (Left e)  = putStrLnStdErr . prettyShow $ e
    ioStuff (Right p) = do
      print p
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

data CompilerErr = Generic String
data Env = Env
  -- Each time a variable is encountered it is mapped to the next number in a
  -- sequence.
  { variables :: Variables
  , maxLabel :: Int
  }

incrLabel :: Compiler Int
incrLabel = do
  lbl <- maxLabel <$> get
  modify (\e -> e { maxLabel = succ lbl })
  return lbl

newLabel :: Compiler LLVM.Label
newLabel = LLVM.Label . ('l':) . show <$> incrLabel

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

runCompiler :: Compiler a -> Either CompilerErr a
runCompiler = runExcept . (`evalStateT` emptyEnv)

compileProg :: Jlt.Prog -> Compiler LLVM.Prog
compileProg = aux . (`evalState` initRS) . alphaRenameProg
  where
    aux p@(Jlt.Program defs) = do
      gVars <- collectGlobals p
      -- NOTE I don't think we actually need to persist the state across the
      -- different topdefs.
      pDefs <- mapM trTopDef defs
      return LLVM.Prog
        { LLVM.pGlobals = gVars
        , LLVM.pDecls   = builtinDecls
        , LLVM.pDefs    = pDefs
        }
    initRS :: RenamerState
    initRS = RS 0 mempty

-- * Alpha renaming

type Renamer a = State RenamerState a

type SymbolTable a b = [Map a b]

data RenamerState = RS Int (SymbolTable Jlt.Ident Jlt.Ident)

alphaRenameProg :: Jlt.Prog -> Renamer Jlt.Prog
alphaRenameProg (Jlt.Program defs) = Jlt.Program <$> mapM alphaRenameTopDefs defs

alphaRenameTopDefs :: Jlt.TopDef -> Renamer Jlt.TopDef
alphaRenameTopDefs (Jlt.FnDef tp id args blk) = undefined

lookupName :: Jlt.Ident -> Renamer (Maybe Jlt.Ident)
lookupName i = (\(RS _ m) -> lookupFirst i m) <$> get





-- | TODO Stub!
collectGlobals :: Jlt.Prog -> Compiler [LLVM.GlobalVar]
collectGlobals _ = return []

trTopDef :: Jlt.TopDef -> Compiler LLVM.Def
trTopDef (Jlt.FnDef t i args blk) = do
  (LLVM.Blk lbl is:bs) <- trBlk blk
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = LLVM.Blk lbl is : bs
    }

trBlk :: Jlt.Blk -> Compiler [LLVM.Blk]
trBlk (Jlt.Block stmts) = do
  lbl <- newLabel
  -- NOTE not sure we should just concat the result of this.
  is  <- concat <$> mapM trStmt stmts
  return [LLVM.Blk lbl is]

trStmt :: Jlt.Stmt -> Compiler [LLVM.Instruction]
trStmt s = case s of
  Jlt.Empty -> todo
  Jlt.BStmt _b -> todo
  -- We should have already taken care of allocating all variables (and
  -- therefore also created a mapping for them)
  Jlt.Decl tp its -> concat <$> mapM (varInit tp) its
  Jlt.Ass _i _e -> todo
  Jlt.Incr i -> incr i
  Jlt.Decr _i -> todo
  Jlt.Ret _e -> todo
  Jlt.VRet -> todo
  Jlt.Cond _e _s0 -> todo
  Jlt.CondElse _e _s0 _s1 -> todo
  Jlt.While _e _s0 -> todo
  Jlt.SExp _e -> todo
  where
    todo = return []

maybeToErr :: MonadError e m => e -> Maybe a -> m a
maybeToErr err Nothing  = throwError err
maybeToErr _   (Just x) = return x

maybeToErr' :: CompilerErr -> Maybe a -> Compiler a
maybeToErr' = maybeToErr

incr :: Jlt.Ident -> Compiler [LLVM.Instruction]
incr i = do
  (xptr, tp) <- undefined -- lookupIdent i >>= maybeToErr' (Generic "Cannot find var")
  (xval, _) <- undefined -- createVar tp
  return
    [ LLVM.Load tp (LLVM.Pointer tp) xptr xval
    , LLVM.Add tp (Left xval) (Right 1) xptr
    ]

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

-- | Assumes that the item is already initialized and exists in scope.
varInit :: Jlt.Type -> Jlt.Item -> Compiler [LLVM.Instruction]
varInit jltType itm = do
  (reg, tp) <- undefined -- lookupItem itm >>= maybeToErr' (Generic "var init - cant find")
  let varInit' :: LLVM.Operand -> Compiler [LLVM.Instruction]
      varInit' op = return . pure $ LLVM.Store tp op (LLVM.Pointer tp) reg
  case itm of
    Jlt.NoInit{} -> varInit' (defaultValue jltType)
    -- Here we must first compute the value of the expression
    -- and store the result of that in the variable.
    Jlt.Init _ e -> do
      (is0, reg0) <- calculateExpression e
      is1 <- varInit' reg0
      return (is0 ++ is1)
  where
    -- For now we will just assume that the expression is `42`.
    calculateExpression :: Jlt.Expr -> Compiler ([LLVM.Instruction], LLVM.Operand)
    calculateExpression _ = return ([], Right 42)

defaultValue :: Jlt.Type -> LLVM.Operand
defaultValue t = case t of
  Jlt.Int -> Right 0
  _   -> todo
  where
    todo = error "default llvm value not yet implemented!"

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
