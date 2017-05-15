module Javalette.Compiler
  ( execAllBackends
  ) where

import Javalette.Syntax as AST
import Javalette.Backend
import qualified Javalette.Backend.LLVM as LLVM

execAllBackends :: FilePath -> AST.Prog -> IO ()
execAllBackends fp p = mapM_ run backends
  where
    run b = runBackend b fp p

backends :: [Backend]
backends = [ LLVM.backend ]
