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
  ) where

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
  pPrint (Name s) = char '@' <+> text s

quoted :: String -> Doc
quoted s = char '"' <+> text s <+> char '"'

data Type
  = Void
  | I32
  | I64

instance Pretty Type where
  pPrint t = case t of
    Void -> text "void"
    I32  -> text "i32"
    I64  -> text "i64"

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
