{- | A wrapper hiding details that only backends need to look at -}
module Javalette.Backend
  ( Internals.Backend
  , Internals.runBackend
  ) where

import qualified Javalette.Backend.Internals as Internals
