module Javalette.Backend.Internals
  ( Backend(..)
  , runBackend
  ) where

import Javalette.Syntax as AST

data Backend = Backend
  { compiler :: AST.Prog -> IO ()
  }

runBackend :: AST.Prog -> Backend -> IO ()
runBackend p b = compiler b p
