module Javalette.Compiler
  ( compile
  ) where

import Javalette.Syntax as AST
import Javalette.Backend
import qualified Javalette.Backend.LLVM as LLVM

compile :: FilePath -> AST.Prog -> IO ()
compile fp p = mapM_ (runBackend fp p) backends

backends :: [Backend]
backends = [ LLVM.backend ]
