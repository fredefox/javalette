{-# LANGUAGE GADTs #-}
module Javalette.Backend.Internals
  ( Backend(..)
  , Backend'(..)
  , mkBackend
  ) where

import Options.Applicative.Types

import Javalette.Syntax as AST

data Backend where
  Backend :: Backend' opts -> Backend

data Backend' opts = Backend'
  { runBackend     :: opts -> FilePath -> AST.Prog -> IO ()
  , backendOptions :: Parser opts
  }

-- | Provides a clean interface to creating a publicly consumable backend, but
-- we lose the nicety of having record syntax
mkBackend
  :: (opts -> FilePath -> Prog -> IO ()) -> Parser opts -> Backend
mkBackend run opts = Backend (Backend' run opts)
