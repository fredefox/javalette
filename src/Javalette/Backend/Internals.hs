{-# LANGUAGE ExistentialQuantification #-}
module Javalette.Backend.Internals
  ( Backend(..)
  ) where

import Options.Applicative

import Javalette.Syntax as AST

-- | A javalette backend. 'opts' is existentially quantified to allow differnt
-- backends to use different options.
data Backend = forall opts . Backend
  { runBackend     :: opts -> FilePath -> AST.Prog -> IO ()
  , backendOptions :: Parser opts
  , enable         :: Mod FlagFields Bool
  }
