module Javalette.Backend.LLVM
  ( backend
  ) where

import Control.Monad.Except
import Control.Monad.State

import Javalette.Backend.Internals
import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint

backend :: Backend
backend = Backend
  { compiler = compile
  }

compile :: Jlt.Prog -> IO ()
compile = runCompiler . compileProg

data CompilerErr
data Env = Env

initEnv :: Env
initEnv = Env

instance Pretty CompilerErr where
  pPrint = error "Unimplemented"

type Compiler a = StateT Env (ExceptT CompilerErr LLVM.Program) a

runCompiler :: Compiler a -> IO ()
runCompiler = handleErrors . runExceptT . (`execStateT` initEnv)

handleErrors :: Pretty e => IO (Either e a) -> IO ()
handleErrors act = act >>= printErr

printErr :: Pretty e => Either e a -> IO ()
printErr e = case e of
  Left err -> prettyPrint err
  Right{}  -> return ()

compileProg :: Jlt.Prog -> Compiler ()
compileProg = undefined
