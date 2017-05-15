

module Javalette.Syntax.AbsJavalette where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Ident [Arg] Blk
  deriving (Eq, Ord, Show, Read)

data Arg = Argument Type Ident
  deriving (Eq, Ord, Show, Read)

data Blk = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Blk
    | Decl Type [Item]
    | Ass LValue Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | For Type Ident Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item
    = NoInit Ident | Init Ident Expr | InitObj Ident Constructor
  deriving (Eq, Ord, Show, Read)

data Constructor = TypeCon Type | ArrayCon Constructor Expr
  deriving (Eq, Ord, Show, Read)

data LValue = LIdent Ident | LIndexed Ident Index
  deriving (Eq, Ord, Show, Read)

data Index = Indx Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = Int | Doub | Bool | Void | Array Type | String | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar Ident
    | ELitInt Integer
    | ELitDoub Double
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | Dot Expr Ident
    | EIndex Expr Index
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | EAnn Type Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)

