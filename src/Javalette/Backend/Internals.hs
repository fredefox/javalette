{-# LANGUAGE ExistentialQuantification #-}
module Javalette.Backend.Internals
  ( Backend(..)
  ) where

import Options.Applicative

import Javalette.Syntax as AST

data Backend = forall opts . Backend
  { runBackend     :: opts -> FilePath -> AST.Prog -> IO ()
  , backendOptions :: Parser opts
  , enable         :: Mod FlagFields Bool
  }
