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
  , Comparison(..)
  , Reg(..)
  , Operand
  , Val
  ) where

import Prelude hiding (EQ)
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
  pPrint (GlobalVar nm tp val)
    = pPrint nm <+> char '=' <+> text "global" <+> pPrint tp <+> pPrint val
  pPrintList _lvl xs = vcat (map pPrint xs)

data Name = Name String deriving (Show)

instance Pretty Name where
  pPrint (Name s) = char '@' <> text s

quoted :: String -> Doc
quoted s = char '"' <+> text s <+> char '"'

data Type
  = Void
  | I Int
  | Double
  | Pointer Type
  | Array Int Type
  deriving (Show)

instance Pretty Type where
  pPrint t = case t of
    Void -> text "void"
    I i  -> text "i" <> text (show i)
    Double -> text "double"
    Pointer t0 -> pPrint t0 <> char '*'
    Array n t' -> brackets (int n <+> char 'x' <+> pPrint t')

data Constant = Constant String deriving (Show)

instance Pretty Constant where
  pPrint (Constant s) = char 'c' <> doubleQuotes (text s)

data Decl = Decl
  { declType :: Type
  , declName :: Name
  , declArgs :: [Type]
  } deriving (Show)

instance Pretty Decl where
  pPrint (Decl tp nm args) = text "declare" <+> pPrint tp <+> pPrint nm
    <> parens (hsepBy (char ',') (map pPrint args))
  pPrintList _lvl xs = vcat (map pPrint xs)

data Def = Def
  { defType :: Type
  , defName :: Name
  , defArgs :: [Type]
  , defBlks :: [Blk]
  } deriving (Show)

hsepBy :: Doc -> [Doc] -> Doc
hsepBy _ [] = mempty
hsepBy _ [x] = x
hsepBy d (x:xs) = x <> d <+> hsepBy d xs

instance Pretty Def where
  pPrint (Def tp nm args blks)
    = text "define"
    <+> pPrint tp
    <+> pPrint nm <> parens (hsepBy (char ',') (map pPrint args))
    <+> lbrace
    $$ vcat (map pPrint blks)
    $$ rbrace
  pPrintList _lvl xs = vcat (map pPrint xs)

data Blk = Blk Label [Instruction] deriving (Show)

instance Pretty Blk where
  pPrint (Blk lbl is) = pPrint lbl <> char ':' <+> vcat (map pPrint is)

data Label = Label String deriving (Show)

instance Pretty Label where
  pPrint (Label s) = text s

data Instruction
  -- * Terminator instructions
  = Return Type Operand
  | VoidReturn
  | Branch Label
  | BranchCond Operand Label Label
  -- * Arithmetic operations, integers
  | Add Type Operand Operand Reg
  | Sub Type Operand Operand Reg
  | Mul Type Operand Operand Reg
  | Div Type Operand Operand Reg
  | Rem Type Operand Operand Reg
  -- * Arithmetic operations, doubles
  | FAdd Type Operand Operand Reg
  | FSub Type Operand Operand Reg
  | FMul Type Operand Operand Reg
  | FDiv Type Operand Operand Reg
  -- * Bitwise operators
  | And Type Operand Operand Reg
  | Or  Type Operand Operand Reg
  -- * Memory access
  | Alloca Type Reg
  | Load Type Type Reg Reg
  | GETELEMTPTR
  | Store Type Operand Type Reg
  -- * Misc.
  | Icmp Comparison Type Operand Operand Reg
  | FCMP
  | Call Type Name [(Type, Operand)] Reg
  | CallVoid Type Name [(Type, Operand)]
  | Unreachable
  | Pseudo String
  deriving (Show)

data Comparison
  = EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE
  deriving (Show)

instance Pretty Comparison where
  pPrint c = text $ case c of
    EQ -> "eq"
    NE -> "ne"
    UGT -> "ugt"
    UGE -> "uge"
    ULT -> "ult"
    ULE -> "ule"
    SGT -> "sgt"
    SGE -> "sge"
    SLT -> "slt"
    SLE -> "sle"

type Operand = Either Reg Val

pPrintOp :: Operand -> Doc
pPrintOp = either pPrint pPrint

type Val = Int

data Reg = Reg String deriving (Show)

instance Pretty Reg where
  pPrint (Reg n) = char '%' <> text n

instance Pretty Instruction where
  pPrint i = case i of
    Branch lbl -> text "br label" <+> pPrint lbl
    BranchCond cond t f
      -> text "br i1" <+> pPrintOp cond <> char ','
      <+> text "label" <+> char '%' <> pPrint t <> char ','
      <+> text "label" <+> char '%' <> pPrint f
    Alloca tp nm -> pPrint nm <+> char '=' <+> text "alloca" <+> pPrint tp
    Load ty0 ty1 regSrc regTrg
      -> pPrint regTrg <+> char '='
      <+> pPrint ty0 <+> char ',' <+> pPrint ty1
      <+> pPrint regSrc
    Store tpOp op tpReg reg
      -> text "store" <+> pPrint tpOp <+> pPrintOp op
      <> char ',' <+> pPrint tpReg <+> pPrint reg
    Return tp op -> text "ret" <+> pPrint tp <+> pPrintOp op
    VoidReturn -> text "ret void"
    Unreachable -> text "unreachable"
    Pseudo s -> char '{' <> text s <> char '}'
    Call t n args r
      -> pPrint r <+> char '='
      <+> text "call" <+> pPrint t <+> pPrint n <> parens (pPrintTypeOp args)
    CallVoid t n args
      -> text "call" <+> pPrint t <+> pPrint n <> parens (pPrintTypeOp args)
    Add t op0 op1 r -> prettyBinInstr (text "add") t op0 op1 r
    Sub t op0 op1 r -> prettyBinInstr (text "sub") t op0 op1 r
    Mul t op0 op1 r -> prettyBinInstr (text "mul") t op0 op1 r
    Div t op0 op1 r -> prettyBinInstr (text "div") t op0 op1 r
    Rem t op0 op1 r -> prettyBinInstr (text "rem") t op0 op1 r
    FAdd t op0 op1 r -> prettyBinInstr (text "fadd") t op0 op1 r
    FSub t op0 op1 r -> prettyBinInstr (text "fsub") t op0 op1 r
    FMul t op0 op1 r -> prettyBinInstr (text "fmul") t op0 op1 r
    FDiv t op0 op1 r -> prettyBinInstr (text "fdiv") t op0 op1 r
    And t op0 op1 r -> prettyBinInstr (text "and") t op0 op1 r
    Or  t op0 op1 r -> prettyBinInstr (text "or") t op0 op1 r
    Icmp cmpr t op0 op1 r -> prettyBinInstr (pPrint cmpr) t op0 op1 r
    _ -> text "{ugly instruction}"

prettyBinInstr
  :: Doc -> Type -> Operand -> Operand -> Reg -> Doc
prettyBinInstr s tp op0 op1 reg
      = pPrint reg <+> char '='
      <+> s <+> pPrint tp
      <+> pPrintOp op0 <> char ',' <+> pPrintOp op1

pPrintTypeOp :: [(Type, Operand)] -> Doc
pPrintTypeOp = hsepBy (char ',') . map (\(t, o) -> pPrint t <+> pPrintOp o)
