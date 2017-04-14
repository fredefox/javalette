{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Javalette.Backend.LLVM.Language
  -- * Syntax
  ( Prog(..)
  , GlobalVar(..)
  , Name(..)
  , Type(..)
  , Constant(..)
  , Decl(..)
  , Def(..)
  , Blk(..)
  , Label(..)
  , Instruction(..)
  -- * Other stuff
  , Program
  , ProgramT
  , MonadProgram
  -- * Constructing a program
  -- * Translating a program
  , translate
  ) where

import Control.Monad.Writer
import Data.Functor.Identity

import Javalette.PrettyPrint

data Prog = Prog
  { pGlobals   :: [GlobalVar]
  , pDecls     :: [Decl]
  , pDefs      :: [Def]
  }

instance Pretty Prog where
  pPrint = undefined

data GlobalVar = GlobalVar
  { gvName :: Name
  , gvType :: Type
  , gvVal  :: Constant
  }

instance Pretty GlobalVar where
  pPrint = undefined

data Name = Name String

instance Pretty Name where
  pPrint = undefined

data Type = Type

instance Pretty Type where
  pPrint = undefined

data Constant = Constant String

instance Pretty Constant where
  pPrint = undefined

data Decl = Decl
  { declType :: Type
  , declName :: Name
  , declArgs :: [Type]
  }

instance Pretty Decl where
  pPrint = undefined

data Def = Def
  { defType :: Type
  , defName :: Name
  , defArgs :: [Type]
  , defBlks :: [Blk]
  }

instance Pretty Def where
  pPrint = undefined

data Blk = Blk Label [Instruction]

instance Pretty Blk where
  pPrint = undefined

data Label = Label String

instance Pretty Label where
  pPrint = undefined

data Instruction
  -- * Terminator instructions
  = RET | BR
  -- * Arithmetic operations, integers
  | ADD | SUB | MUL | SDIV | SREM
  -- * Arithmetic operations, doubles
  | FADD | FSUB | FMUL | FDIV
  -- * Memory access
  | ALLOCA | LOAD | GETELEMTPTR | STORE
  -- * Misc.
  | ICMP | FCMP | CALL

instance Pretty Instruction where
  pPrint i = case i of
    _ -> undefined
  pPrintList lvl xs = undefined


progWith :: Monad m => [GlobalVar] -> [Decl] -> m [Def] -> m Prog
progWith gv decls act = Prog gv decls <$> act

type ProgramT = WriterT [Instruction]
type Program = ProgramT Identity
type MonadProgram = MonadWriter Instruction

-- | Outputs the concrete syntax corresponding to this program.
translate :: Program a -> String
translate = prettyShow . execWriter
