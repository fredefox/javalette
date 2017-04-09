module Javalette.Compiler
  ( compile
  ) where

import Javalette.Syntax as AST
import Javalette.Backend
import qualified Javalette.Backend.LLVM as LLVM

compile :: AST.Prog -> IO ()
compile p = mapM_ (runBackend p) backends

backends :: [Backend]
backends = [ LLVM.backend ]
