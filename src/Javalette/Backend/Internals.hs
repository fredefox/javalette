{-# LANGUAGE ExistentialQuantification #-}
module Javalette.Backend.Internals
  ( Backend(..)
  ) where

import Options.Applicative.Types

import Javalette.Syntax as AST
import qualified Javalette.Options as StdOpts

data Backend = forall opts . Backend
  { runBackend     :: opts -> FilePath -> AST.Prog -> IO ()
  , backendOptions :: ParserInfo (StdOpts.Args opts)
  }
