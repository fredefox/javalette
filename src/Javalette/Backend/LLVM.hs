module Javalette.Backend.LLVM
  ( backend
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import System.IO

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
    ioStuff (Right p) = writeFile fp (prettyShow p)

putStrLnStdErr :: String -> IO ()
putStrLnStdErr = hPutStrLn stderr

data CompilerErr = Generic String
data Env = Env
  { uniqId :: Int
  }

-- | Gets the current id and increments it
incrId :: Compiler Int
incrId = do
  id <- uniqId <$> get
  modify (\e -> e { uniqId = succ (uniqId e) })
  return id

-- | Gets and increments the unique id, prepends a 'v' and converts it to a
-- name.
getUniqName :: Compiler LLVM.Name
getUniqName = LLVM.Name . ('v':) . show <$> incrId

initEnv :: Env
initEnv = Env 0

instance Pretty CompilerErr where
  pPrint err = case err of
    Generic s -> text "ERR:" <+> text s

type Compiler a = StateT Env (Except CompilerErr) a

runCompiler :: Compiler a -> Either CompilerErr a
runCompiler = runExcept . (`evalStateT` initEnv)

compileProg :: Jlt.Prog -> Compiler LLVM.Prog
compileProg (Jlt.Program defs) = do
  gVars <- _collectGlobals
  let decls = map decl defs
  pDefs <- mapM collectDef defs
  return $ LLVM.Prog
    { LLVM.pGlobals = gVars
    , LLVM.pDecls   = decls ++ builtinDecls
    , LLVM.pDefs    = pDefs
    }
  where
    _collectGlobals = undefined

collectDef :: Jlt.TopDef -> Compiler LLVM.Def
collectDef (Jlt.FnDef t i args blk) = return LLVM.Def
  { LLVM.defType = trType t
  , LLVM.defName = undefined
  , LLVM.defArgs = undefined
  , LLVM.defBlks = undefined
  }

trType :: Jlt.Type -> LLVM.Type
trType t = case t of
  Jlt.Int -> LLVM.I64
  Jlt.Doub -> undefined
  Jlt.Bool -> undefined
  Jlt.Void -> undefined
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

decl :: Jlt.TopDef -> LLVM.Decl
decl (Jlt.FnDef tp i arg blk) = LLVM.Decl
  { LLVM.declType = undefined
  , LLVM.declName = undefined
  , LLVM.declArgs = undefined
  }
