{- | Helper methods for debugging inside Javalette -}
module Javalette.Debug
  ( module Debug.Trace
  , tracePretty
  , tracePrettyId
  ) where

import Debug.Trace

import Javalette.PrettyPrint

-- | Pretty version of 'traceShow.
tracePretty :: Pretty a => a -> b -> b
tracePretty x = trace (prettyShow x)

-- | Pretty version of 'traceShowId'.
tracePrettyId :: Pretty a => a -> a
tracePrettyId x = tracePretty x x
