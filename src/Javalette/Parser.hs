{-# LANGUAGE FlexibleContexts #-}
module Javalette.Parser ( Parseable(..) ) where

import Control.Monad.Except

import Javalette.Syntax.AbsJavalette
import Javalette.Syntax.LexJavalette ( Token )
import Javalette.Syntax.ParJavalette
import Javalette.Syntax.ErrM

class Parseable a where
  parse :: MonadError String m => String -> m a

instance Parseable Prog where
  parse = wrapParse pProg

-- instance Parseable TopDef where
--   parse = wrapParse pTopDef

-- instance Parseable Arg where
--   parse = wrapParse pArg

-- instance Parseable Blk where
--   parse = wrapParse pBlk

-- instance Parseable Stmt where
--   parse = wrapParse pStmt

-- instance Parseable Item where
--   parse = wrapParse pItem

-- instance Parseable Type where
--   parse = wrapParse pType

-- instance Parseable Expr where
--   parse = wrapParse pExpr

-- instance Parseable AddOp where
--   parse = wrapParse pAddOp

-- instance Parseable MulOp where
--   parse = wrapParse pMulOp

-- instance Parseable RelOp where
--   parse = wrapParse pRelOp


wrapErr :: MonadError String m => Err a -> m a
wrapErr (Ok a) = return a
wrapErr (Bad s) = throwError s

wrapParse :: MonadError String m
  => ([Token] -> Err a) -- ^ The parser
  -> String             -- ^ The string to parse
  -> m a
wrapParse p = wrapErr . p . myLexer
