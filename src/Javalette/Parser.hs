{- | A simple class-wrapper around auto-generated bnfc-stuff. -}
{-# LANGUAGE FlexibleContexts #-}
module Javalette.Parser ( Parseable(..) ) where

import Control.Exception
import Control.Monad.Except

import Javalette.Syntax.AbsJavalette
import Javalette.Syntax.LexJavalette ( Token )
import Javalette.Syntax.ParJavalette
import Javalette.Syntax.ErrM

-- | This class defines stuff that can be parsed.
class Parseable a where
  -- | Tries to parse a string to the target `Parseable` and throws an
  -- exception (in `MonadError`) if this is unsuccessful.
  parse :: MonadError ParseError m => String -> m a

-- | Programs are the only thing that is parseable
instance Parseable Prog where
  parse = wrapParse pProg

-- | Generalize `Err` to any `MonadError`.
liftErr :: MonadError ParseError m => Err a -> m a
liftErr (Ok a) = return a
liftErr (Bad s) = throwError (ParseError s)

newtype ParseError = ParseError String deriving (Show)

instance Exception ParseError where
  displayException (ParseError s) = show s

-- | Wraps a parsing function.
wrapParse :: MonadError ParseError m
  => ([Token] -> Err a) -- ^ The parser
  -> String             -- ^ The string to parse
  -> m a
wrapParse p = liftErr . p . myLexer
