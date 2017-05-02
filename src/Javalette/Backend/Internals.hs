module Javalette.Backend.Internals
  ( Backend(..)
  , runBackend
  ) where

import Javalette.Syntax as AST

newtype Backend = Backend
  { compiler :: FilePath -> AST.Prog -> IO ()
  }

runBackend :: FilePath -> AST.Prog -> Backend -> IO ()
runBackend fp p b = compiler b fp p
