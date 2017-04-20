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
type Closure a b = Map a b
-- | The first component represents the next variable name to be used.
type Scope = (Int, [Closure Jlt.Ident (LLVM.Reg, LLVM.Type)])

emptyClosure :: Ord a => Closure a b
emptyClosure = mempty

emptyScope :: Scope
emptyScope = (0, [])

-- | Assigns a name to `a` in the current closure and returns it. Do not call
-- `newVar` on a variable that has already been assigned or on a scope with no
-- closures.
mapVar :: Jlt.Ident -> Jlt.Type -> Variables -> (Variables, (LLVM.Reg, LLVM.Type))
mapVar _ _ (_, []) = error "Cannot map variables with no closures at hand"
mapVar a t (i, c:cs) = ((succ i, M.insert a m c : cs), m)
  where
    m :: (LLVM.Reg, LLVM.Type)
    m = (intToName i, trType t)

newVar :: LLVM.Type -> Variables -> (Variables, (LLVM.Reg, LLVM.Type))
newVar _ (_, []) = error "Cannot create variables with no closures at hand"
newVar t (i, c:cs) = ((succ i, M.insert anon m c : cs), m)
  where
    m = (intToName i, t)
    -- This is ugly! We may wanna change it so that we have a seperate map for
    -- going from jlt variable names to llvm variable names and another one for
    -- looking up info about llvm varibles. The reason for doing this would be
    -- that not all llvm variables will come from a jlt variable.
    anon = undefined

-- | Gets whatever `a` is bound to.
getVar :: Jlt.Ident -> Variables -> Maybe (LLVM.Reg, LLVM.Type)
getVar a (_, cs) = lookupFirst a cs

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
getVariables = variables <$> get

assignName :: Jlt.Ident -> Jlt.Type -> Compiler (LLVM.Reg, LLVM.Type)
assignName i t = do
  vars <- getVariables
  let (s, res) = mapVar i t vars
  modify (\e -> e { variables = s })
  return res

createVar :: LLVM.Type -> Compiler (LLVM.Reg, LLVM.Type)
createVar t = do
  vars <- getVariables
  let (s, res) = newVar t vars
  modify (\e -> e { variables = s })
  return res

lookupIdent :: Jlt.Ident -> Compiler (Maybe (LLVM.Reg, LLVM.Type))
lookupIdent i = do
  (_, ms) <- getVariables
  return $ lookupFirst i ms

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
compileProg p@(Jlt.Program defs) = do
  gVars <- collectGlobals p
  -- NOTE I don't think we actually need to persist the state across the
  -- different topdefs.
  pDefs <- mapM trTopDef defs
  return LLVM.Prog
    { LLVM.pGlobals = gVars
    , LLVM.pDecls   = builtinDecls
    , LLVM.pDefs    = pDefs
    }

-- | TODO Stub!
collectGlobals :: Jlt.Prog -> Compiler [LLVM.GlobalVar]
collectGlobals _ = return []

trTopDef :: Jlt.TopDef -> Compiler LLVM.Def
trTopDef (Jlt.FnDef t i args blk) = do
  vars <- varsBlk blk
  (LLVM.Blk lbl is:bs) <- trBlk blk
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = LLVM.Blk lbl (vars ++ is) : bs
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
  Jlt.BStmt b -> todo
  Jlt.Decl t its -> todo
  Jlt.Ass i e -> todo
  Jlt.Incr i -> incr i
  Jlt.Decr i -> todo
  Jlt.Ret e -> todo
  Jlt.VRet -> todo
  Jlt.Cond e s0 -> todo
  Jlt.CondElse e s0 s1 -> todo
  Jlt.While e s0 -> todo
  Jlt.SExp e -> todo
  where
    todo = return []

maybeToErr :: MonadError e m => e -> Maybe a -> m a
maybeToErr err Nothing  = throwError err
maybeToErr _   (Just x) = return x

maybeToErr' :: CompilerErr -> Maybe a -> Compiler a
maybeToErr' = maybeToErr

incr :: Jlt.Ident -> Compiler [LLVM.Instruction]
incr i = do
  (xptr, tp) <- lookupIdent i >>= maybeToErr' (Generic "Cannot find var")
  (xval, _) <- createVar tp
  return
    [ LLVM.Load tp (LLVM.Pointer tp) xptr xval
    , LLVM.Add tp (Left xval) (Right 1) xptr
    ]

pushScope, popScope :: Compiler ()
pushScope = modify (\e -> e { variables = push . variables $ e })
  where
    push :: Variables -> Variables
    push (m, xs) = (m, emptyClosure : xs)

popScope = modify (\e -> e { variables = pop . variables $ e })
  where
    pop :: Variables -> Variables
    pop (_, []) = error "Cannot pop on empty environment"
    pop (m, _:xs) = (m, xs)

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
  (reg, tp) <- assignName (itemName itm) t
  return (LLVM.Alloca tp reg)

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
