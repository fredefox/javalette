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
type Scope a b = (b, [Closure a b])

emptyClosure :: Ord a => Closure a b
emptyClosure = mempty

emptyScope :: Enum b => Scope a b
emptyScope = (toEnum 0, [])

-- | Assigns a name to `a` in the current closure and returns it. Do not call
-- `newVar` on a variable that has already been assigned or on a scope with no
-- closures.
newVar :: (Ord a, Enum b) => a -> Scope a b -> (Scope a b, b)
newVar _ (_, []) = error "Cannot bind variables with no closures at hand"
newVar a (m, c:cs) = ((succ m, M.insert a m c : cs), m)

-- | Gets whatever `a` is bound to.
getVar :: Ord a => a -> Scope a b -> Maybe b
getVar a (_, cs) = lookupFirst a cs

lookupFirst :: Ord a => a -> [Map a b] -> Maybe b
lookupFirst _ [] = undefined
lookupFirst a (x : xs) = case M.lookup a x of
  Nothing -> lookupFirst a xs
  Just b  -> return b

type Variables = Scope Jlt.Ident Int

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

assignName :: Jlt.Ident -> Compiler LLVM.VarName
assignName i = do
  vars <- variables <$> get
  let (s, v) = newVar i vars
  modify (\e -> e { variables = s })
  return (intToName v)

intToName :: Int -> LLVM.VarName
intToName = LLVM.VarName . ('v':) . show

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
  let decls = map defToDecl defs
  -- NOTE I don't think we actually need to persist the state across the
  -- different topdefs.
  pDefs <- mapM trTopDef defs
  return LLVM.Prog
    { LLVM.pGlobals = gVars
    , LLVM.pDecls   = builtinDecls ++ decls
    , LLVM.pDefs    = pDefs
    }

-- | TODO Stub!
collectGlobals :: Jlt.Prog -> Compiler [LLVM.GlobalVar]
collectGlobals _ = return []

trTopDef :: Jlt.TopDef -> Compiler LLVM.Def
trTopDef (Jlt.FnDef t i args blk) = do
  entry <- newLabel
  vars <- varsBlk blk
  todo <- morestuff blk
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = LLVM.Blk entry vars : todo
    }
    where
      morestuff _blk = return []

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
  nm <- assignName (itemName itm)
  return (LLVM.Alloca (trType t) nm)

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

defToDecl :: Jlt.TopDef -> LLVM.Decl
defToDecl (Jlt.FnDef tp i arg blk) = LLVM.Decl
  { LLVM.declType = trType tp
  , LLVM.declName = trName i
  , LLVM.declArgs = map trArg arg
  }
