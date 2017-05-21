module Javalette.Compiler
  ( runBackends
  ) where

import Javalette.Syntax as AST
import Javalette.Backend ( Backend )
import qualified Javalette.Backend as Backend
import qualified Javalette.Backend.LLVM as LLVM

runBackends :: (FilePath -> IO AST.Prog) -> IO ()
runBackends = Backend.runBackends backends

backends :: [Backend]
backends = [ LLVM.backend ]
