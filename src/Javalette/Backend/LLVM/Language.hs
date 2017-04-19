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
  , VarName(..)
  ) where

import Javalette.PrettyPrint

data Prog = Prog
  { pGlobals   :: [GlobalVar]
  , pDecls     :: [Decl]
  , pDefs      :: [Def]
  } deriving (Show)

instance Pretty Prog where
  pPrint (Prog gVars decls defs) = pPrint gVars $$ pPrint decls $$ pPrint defs

data GlobalVar = GlobalVar
  { gvName :: Name
  , gvType :: Type
  , gvVal  :: Constant
  } deriving (Show)

instance Pretty GlobalVar where
  pPrint (GlobalVar nm tp val) = pPrint nm <+> pPrint tp <+> pPrint val
  pPrintList _lvl xs = vcat (map pPrint xs)

data Name = Name String deriving (Show)

instance Pretty Name where
  pPrint (Name s) = char '@' <> text s

quoted :: String -> Doc
quoted s = char '"' <+> text s <+> char '"'

data Type
  = Void
  | I32
  | I64
  deriving (Show)

instance Pretty Type where
  pPrint t = case t of
    Void -> text "void"
    I32  -> text "i32"
    I64  -> text "i64"

data Constant = Constant String deriving (Show)

instance Pretty Constant where
  pPrint (Constant s) = text s

data Decl = Decl
  { declType :: Type
  , declName :: Name
  , declArgs :: [Type]
  } deriving (Show)

instance Pretty Decl where
  pPrint (Decl tp nm args) = text "declare" <+> pPrint tp <+> pPrint nm <> parens (hsep (map pPrint args))
  pPrintList _lvl xs = vcat (map pPrint xs)

data Def = Def
  { defType :: Type
  , defName :: Name
  , defArgs :: [Type]
  , defBlks :: [Blk]
  } deriving (Show)

instance Pretty Def where
  pPrint (Def tp nm args blks)
    = text "define"
    <+> pPrint tp
    <+> pPrint nm <> parens (hsep (map pPrint args)) <+> lbrace
    $$ vcat (map pPrint blks)
    $$ rbrace
  pPrintList _lvl xs = vcat (map pPrint xs)

data Blk = Blk Label [Instruction] deriving (Show)

instance Pretty Blk where
  pPrint (Blk lbl is) = pPrint lbl <+> vcat (map pPrint is)

data Label = Label String deriving (Show)

instance Pretty Label where
  pPrint (Label s) = text (s ++ ":")

data Instruction
  -- * Terminator instructions
  = RET | BR
  -- * Arithmetic operations, integers
  | ADD | SUB | MUL | SDIV | SREM
  -- * Arithmetic operations, doubles
  | FADD | FSUB | FMUL | FDIV
  -- * Memory access
  | Alloca Type VarName | LOAD | GETELEMTPTR | STORE
  -- * Misc.
  | ICMP | FCMP | CALL
  deriving (Show)

data VarName = VarName String deriving (Show)

instance Pretty VarName where
  pPrint (VarName n) = char '%' <> text n

instance Pretty Instruction where
  pPrint i = case i of
    Alloca tp nm -> pPrint nm <+> char '=' <+> text "alloca" <+> pPrint tp
    _ -> text "instruction"
--  pPrintList lvl xs = undefined
