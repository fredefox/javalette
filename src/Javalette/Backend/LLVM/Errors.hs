{- | Various runtime errors -}
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

-- | Something that can never happen.
{-# INLINE impossible #-}
impossible :: String -> a
impossible = E.throw . Impossible

-- | A type-error
{-# INLINE typeerror #-}
typeerror :: String -> a
typeerror = E.throw . TypeError

-- | Errors that cannot happen because they have been desugared by the
-- type-checker.
{-# INLINE impossibleRemoved #-}
impossibleRemoved :: a
impossibleRemoved = typeerror "removed by typechecker"
