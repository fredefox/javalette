module Javalette.Debug
  ( tracePretty
  , tracePrettyId
  ) where

import Debug.Trace

import Javalette.PrettyPrint

tracePretty :: Pretty a => a -> b -> b
tracePretty x = trace (prettyShow x)

tracePrettyId :: Pretty a => a -> a
tracePrettyId x = tracePretty x x
