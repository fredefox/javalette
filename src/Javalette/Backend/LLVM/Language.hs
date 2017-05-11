{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Javalette.Backend.LLVM.Language
  (
  -- * Syntax
    Prog(..)
  , GlobalVar(..)
  , Name(..)
  , Type(..)
  , Constant(..)
  , Decl(..)
  , Def(..)
  , Arg(..)
  , Blk(..)
  , Label(..)
  , TermInstr(..)
  , Instruction(..)
  , Comparison(..)
  , Reg(..)
  , Operand
  , Val(..)
  , Op(..)
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

newtype Name = Name String deriving (Show)

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

newtype Constant = Constant String deriving (Show)

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

data Arg = Arg Type Reg deriving (Show)

instance Pretty Arg where
  pPrint (Arg t n) = pPrint t <+> pPrint n

data Def = Def
  { defType :: Type
  , defName :: Name
  , defArgs :: [Arg]
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

data Blk = Blk Label [Instruction] TermInstr [TermInstr] deriving (Show)

instance Pretty Blk where
  pPrint (Blk lbl is ti ti'') = pPrint lbl <> char ':' <+> vcat (map pPrint is ++ [pPrint ti] ++ map pPrint ti'')

newtype Label = Label String deriving (Show)

instance Pretty Label where
  pPrint (Label s) = text s

data TermInstr
  -- | Terminator instructions
  = Return Type Operand
  | VoidReturn
  | Branch Label
  | BranchCond Operand Label Label
  | Unreachable
  | CommentedT TermInstr
  deriving (Show)

data Instruction
  = BinOp Op Type Operand Operand Reg
  -- | Memory access
  | Alloca Type Reg
  | Load Type Type Reg Reg
  | GetElementPtr Type Type Name [(Type, Int)] Reg
  | Store Type Operand Type Reg
  -- | Misc.
  | Icmp Comparison Type Operand Operand Reg
  | Fcmp Comparison Type Operand Operand Reg
  | Call Type Name [(Type, Operand)] Reg
  | CallVoid Type Name [(Type, Operand)]
  | Commented Instruction
  | Comment String
  deriving (Show)

data Op
  -- | Arithmetic operations, integers
  = Add | Sub | Mul | SDiv | SRem
  -- | Arithmetic operations, doubles
  | FAdd | FSub | FMul | FDiv
  -- | Bitwise operators
  | And | Or | Xor
  deriving (Show)

-- TODO Split up comparisons in those that are integer-based and those that are
-- not.
data Comparison
  = EQ | NE | UGT | UGE | ULT | ULE | SGT | SGE | SLT | SLE
  | OEQ | ONE | OLT | OLE | OGT | OGE
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
    OEQ -> "oeq"
    OLT -> "olt"
    OLE -> "ole"
    OGT -> "ogt"
    OGE -> "oge"
    ONE -> "one"

type Operand = Either Reg Val

pPrintOp :: Operand -> Doc
pPrintOp = either pPrint pPrint

data Val = ValInt Int | ValDoub Double deriving (Show)

instance Pretty Val where
  pPrint v = case v of
    ValInt i -> int i
    ValDoub d -> double d

newtype Reg = Reg String deriving (Show)

instance Pretty Reg where
  pPrint (Reg n) = char '%' <> text n

instance Pretty Instruction where
  pPrint i = case i of
    Alloca tp nm -> pPrint nm <+> char '=' <+> text "alloca" <+> pPrint tp
    Load ty0 ty1 regSrc regTrg
      -> pPrint regTrg <+> char '=' <+> text "load"
      <+> pPrint ty0 <> char ',' <+> pPrint ty1 <+> pPrint regSrc
    Store tpOp op tpReg reg
      -> text "store" <+> pPrint tpOp <+> pPrintOp op
      <> char ',' <+> pPrint tpReg <+> pPrint reg
    Call t n args r
      -> pPrint r <+> char '='
      <+> text "call" <+> pPrint t <+> pPrint n <> parens (pPrintTypeOp args)
    CallVoid t n args
      -> text "call" <+> pPrint t <+> pPrint n <> parens (pPrintTypeOp args)
    BinOp op t op0 op1 r -> pPrint r <+> char '='
      <+> pPrint op <+> pPrint t
      <+> pPrintOp op0 <> char ',' <+> pPrintOp op1
    Icmp cmpr t op0 op1 r -> prettyBinInstr (text "icmp" <+> pPrint cmpr) t op0 op1 r
    Fcmp cmpr t op0 op1 r -> prettyBinInstr (text "fcmp" <+> pPrint cmpr) t op0 op1 r
  -- <result> = getelementptr <ty>, <ty>* <ptrval>{, [inrange] <ty> <idx>}*
    GetElementPtr tp0 tp1 nm args trg ->
      pPrint trg <+> char '=' <+>
      text "getelementptr" <+> pPrint tp0 <> char ',' <+> pPrint tp1 <+>
      pPrint nm <> char ',' <+> printArgs args
    Commented i' -> char ';' <+> pPrint i'
    Comment s -> char ';' <+> text s

instance Pretty Op where
  pPrint op = text $ case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    SDiv -> "sdiv"
    SRem -> "srem"
    FAdd -> "fadd"
    FSub -> "fsub"
    FMul -> "fmul"
    FDiv -> "fdiv"
    And -> "and"
    Or -> "or"
    Xor -> "xor"

printArgs :: [(Type, Int)] -> Doc
printArgs = hsepBy (char ',') . map arg
  where
    arg (t, i) = pPrint t <+> int i

instance Pretty TermInstr where
  pPrint i = case i of
    Branch lbl -> text "br label" <+> char '%' <> pPrint lbl
    BranchCond cond t f
      -> text "br i1" <+> pPrintOp cond <> char ','
      <+> text "label" <+> char '%' <> pPrint t <> char ','
      <+> text "label" <+> char '%' <> pPrint f
    Return tp op -> text "ret" <+> pPrint tp <+> pPrintOp op
    VoidReturn -> text "ret void"
    Unreachable -> text "unreachable"
    CommentedT i' -> char ';' <+> pPrint i'

prettyBinInstr
  :: Doc -> Type -> Operand -> Operand -> Reg -> Doc
prettyBinInstr s tp op0 op1 reg
      = pPrint reg <+> char '='
      <+> s <+> pPrint tp
      <+> pPrintOp op0 <> char ',' <+> pPrintOp op1

pPrintTypeOp :: [(Type, Operand)] -> Doc
pPrintTypeOp = hsepBy (char ',') . map (\(t, o) -> pPrint t <+> pPrintOp o)
