{- | Simple wrapper around `Text.PrettyPrint.HughesPJClass` that defines `prettyPrint` -}
module Javalette.PrettyPrint
  ( module Text.PrettyPrint.HughesPJClass
  , prettyPrint
  ) where

import Text.PrettyPrint.HughesPJClass

-- | Print the result of `prettyShow`.
prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . prettyShow
