{- | A simple class-wrapper around auto-generated bnfc-stuff. -}
{-# LANGUAGE FlexibleContexts #-}
module Javalette.Parser ( Parseable(..) ) where

import Control.Monad.Except

import Javalette.Syntax.AbsJavalette
import Javalette.Syntax.LexJavalette ( Token )
import Javalette.Syntax.ParJavalette
import Javalette.Syntax.ErrM

-- | This class defines stuff that can be parsed.
class Parseable a where
  -- | Tries to parse a string to the target `Parseable` and throws an
  -- exception (in `MonadError`) if this is unsuccessful.
  parse :: MonadError String m => String -> m a

-- | Programs are the only thing that is parseable
instance Parseable Prog where
  parse = wrapParse pProg

-- | Generalize `Err` to any `MonadError`.
liftErr :: MonadError String m => Err a -> m a
liftErr (Ok a) = return a
liftErr (Bad s) = throwError s

-- | Wraps a parsing function.
wrapParse :: MonadError String m
  => ([Token] -> Err a) -- ^ The parser
  -> String             -- ^ The string to parse
  -> m a
wrapParse p = liftErr . p . myLexer
