module Javalette.Backend.LLVM.Errors
  ( CompilerErr(..)
  , impossible
  , typeerror
  , impossibleRemoved
  ) where

import qualified Control.Exception as E
import Javalette.PrettyPrint

-- | A compiler error.
data CompilerErr = Generic String | Impossible String | TypeError String

instance Show CompilerErr where
  show e = case e of
    Generic s -> "ERROR: " ++ s
    Impossible s -> "THE IMPOSSIBLE HAPPENED: " ++ s
    TypeError s -> "TYPE ERROR: " ++ s

instance E.Exception CompilerErr where

instance Pretty CompilerErr where
  pPrint err = text "ERR:" <+> case err of
    Generic s -> text s
    Impossible s -> text s
    TypeError s -> text s

-- Various runtime errors

{-# INLINE impossible #-}
impossible :: String -> a
impossible = E.throw . Impossible

{-# INLINE typeerror #-}
typeerror :: String -> a
typeerror = E.throw . TypeError

{-# INLINE impossibleRemoved #-}
impossibleRemoved :: a
impossibleRemoved = typeerror "removed by typechecker"
