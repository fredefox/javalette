{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
module Javalette.Interpreter.Program
  ( Program
  , MonadProgram
  -- * Constructing a program
  , printInt
  , printDouble
  , printString
  , readInt
  , readDouble
  -- * Running a program
  , interpret
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Free

data Instruction next
  = PrintInt    Integer next
  | PrintDouble Double  next
  | PrintString String  next
  | ReadInt     (Integer -> next)
  | ReadDouble  (Double  -> next)
  deriving (Functor)

type Program = Free Instruction
type MonadProgram = MonadFree Instruction

printInt :: MonadProgram m => Integer -> m ()
printInt x = liftF (PrintInt x ())

printDouble :: MonadProgram m => Double -> m ()
printDouble x = liftF (PrintDouble x ())

printString :: MonadProgram m => String -> m ()
printString x = liftF (PrintString x ())

readInt :: MonadProgram m => m Integer
readInt = liftF (ReadInt id)

readDouble :: MonadProgram m => m Double
readDouble = liftF (ReadDouble id)

interpret :: Program a -> IO a
interpret p = case p of
  Pure a -> return a
  Free (PrintInt    i next) -> print i >> interpret next
  Free (PrintDouble d next) -> print d >> interpret next
  Free (PrintString s next) -> putStrLn s >> interpret next
  Free (ReadInt     f     ) -> readLn >>= interpret . f
  Free (ReadDouble  f     ) -> readLn >>= interpret . f

