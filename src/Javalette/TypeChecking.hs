{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Javalette.TypeChecking
  ( runTypeChecker
  , evalTypeChecker
  , TypeChecker()
  , TypeCheck(..)
  , TypeCheckingError(..)
  , Infer(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State

import Javalette.Syntax.AbsJavalette
import qualified Javalette.Syntax.AbsJavalette as AST

-- * Type errors

-- | Defines a type error
data TypeCheckingError
  = EmptyEnvironment
  | Uninitialized
  | TypeMismatch
  | NoReturnStatement
  | Undef
  | GenericError String
  deriving (Show)

-- * Type- checking and -inference

-- | This class defines something that can be type-checked.
class TypeCheck a where
  -- TODO: The spec has this comment which I have ignored for now:
  --
  -- > One thing to note is that it may be useful to implement the type-checker as
  -- > a function, which traverses the syntax *and returns its input* if the program
  -- > is type-correct. [spec]
  --
  -- [spec]: http://www.cse.chalmers.se/edu/course/TDA283/project/
  --
  -- TODO: Would be nice to generalize this infterface to not use `TypeChecker`
  -- but use suitable constraints like MonadError etc..
  typecheck :: a -> TypeChecker ()
  default typecheck :: Infer a => a -> TypeChecker ()
  typecheck = void . infer

-- | This class defines something whose type can be inferred.
class TypeCheck a => Infer a where
  infer :: a -> TypeChecker Type

data Env = Env
  { envVars :: [Map Ident Type]
  , envDefs :: Definitions
  }

type Definitions = Map Ident Definition
data Definition = DefWiredIn WiredIn | Def TopDef

modifyVars :: ([Map Ident Type] -> [Map Ident Type]) -> Env -> Env
modifyVars f e = e { envVars = f (envVars e) }

modifyDefs :: (Definitions -> Definitions) -> Env -> Env
modifyDefs f e = e { envDefs = f (envDefs e) }

type TypeChecker a = StateT Env (Except TypeCheckingError) a

runTypeChecker
  :: TypeChecker a
  -> Either TypeCheckingError (a, Env)
runTypeChecker t = runExcept . runStateT t $ initEnv

evalTypeChecker :: TypeChecker a -> Either TypeCheckingError a
evalTypeChecker t = runExcept . evalStateT t $ initEnv

initEnv :: Env
initEnv = Env
  { envVars = []
  , envDefs = wiredInDefs
  }

data WiredIn = WiredIn
  { wiredInType :: Type
  , wiredInArgs :: [Arg]
  , wiredInIdent :: Ident
  }

wiredInDefs :: Definitions
wiredInDefs = M.fromList
  [ "printInt"    |-> WiredIn Void      [Argument AST.Int    (Ident "_")] (Ident "printInt")
  , "printDouble" |-> WiredIn Void      [Argument AST.Doub   (Ident "_")] (Ident "printDouble")
  , "printString" |-> WiredIn Void      [Argument AST.String (Ident "_")] (Ident "printString")
  , "readInt"     |-> WiredIn AST.Int   []                                (Ident "readInt")
  , "readDouble"  |-> WiredIn AST.Doub  []                                (Ident "readDouble")
  ]
  where
    i |-> def = (Ident i, DefWiredIn def)

newScope :: TypeChecker ()
newScope = modify (modifyVars $ \xs -> M.empty : xs)

unionDefs :: Definitions -> TypeChecker ()
unionDefs defs = modify (modifyDefs $ M.union defs)

addBinding :: Ident -> Type -> TypeChecker ()
addBinding i t = do
  e <- get
  case envVars e of
    []      -> throwError EmptyEnvironment
    x : xs  -> put $ e { envVars = M.insert i t x : xs }

addBindings :: [(Ident, Type)] -> TypeChecker ()
addBindings = mapM_ (uncurry addBinding)

addArgs :: [Arg] -> TypeChecker ()
addArgs = addBindings . map identAndType
  where
    identAndType (Argument t i) = (i, t)

mkFunEnv :: [TopDef] -> Definitions
mkFunEnv = M.fromList . map (\def@(FnDef _ i _ _) -> (i, Def def))

instance TypeCheck Prog where
  typecheck (Program defs)
    =  unionDefs (mkFunEnv defs)
    >> mapM_ typecheck defs

instance TypeCheck TopDef where
  typecheck (FnDef t _ args blk) = do
    _ <- newScope
    _ <- addArgs args
    typecheckBlk t blk

-- I don't like that type-checking does not have the same interface for all
-- parts of the language. A solution to this would be to expand state to also
-- carry around the "current expected type" but this is also not so elegant
-- because there are many situations where that will then not be needed.
-- instance TypeCheck Blk where
typecheckBlk :: Type -> Blk -> TypeChecker ()
typecheckBlk t (Block stms) = mapM_ (typecheckStmt t) stms

-- instance TypeCheck Stmt where
typecheckStmt :: Type -> Stmt -> TypeChecker ()
typecheckStmt t s = case s of
  Empty          -> return ()
  BStmt blk      -> newScope >> typecheckBlk t blk
  Decl t0 its    -> do
    mapM_ (typecheckItem t0) its
    addBindings $ map (\i -> (itemIdent i, t0)) its
  Ass i e        -> do
    ti <- lookupTypeVar i
    te <- infer e
    unless (ti == te)
      $ throwError TypeMismatch
  Incr i         -> do
    t' <- lookupTypeVar i
    unless (isNumeric t')
      $ throwError TypeMismatch
  Decr i         -> do
    t' <- lookupTypeVar i
    unless (isNumeric t')
      $ throwError TypeMismatch
  Ret e          -> do
    t' <- infer e
    unless (t == t')
      $ throwError TypeMismatch
  VRet           -> unless (t == Void) $ throwError TypeMismatch
  Cond e s0      -> do
    inferBoolean e
    typecheckStmt t s0
  CondElse e s0 s1 -> do
    inferBoolean e
    typecheckStmt t s0
    typecheckStmt t s1
  While e s0 -> do
    inferBoolean e
    typecheckStmt t s0
  SExp e -> typecheck e

inferBoolean :: Expr -> TypeChecker ()
inferBoolean e = do
  te <- infer e
  unless (te == AST.Bool)
    $ throwError TypeMismatch

maybeToError :: MonadError e m => e -> Maybe a -> m a
maybeToError err Nothing  = throwError err
maybeToError _   (Just a) = return a

lookupTypeVar :: Ident -> TypeChecker Type
lookupTypeVar i = do
  t <- firstMatch . envVars <$> get
  maybeToError Uninitialized t
    where
      firstMatch :: [Map Ident Type] -> Maybe Type
      firstMatch []       = Nothing
      firstMatch (m : ms) = case M.lookup i m of
        Nothing -> firstMatch ms
        Just t  -> Just t

lookupFun :: Ident -> TypeChecker Definition
lookupFun i = do
  t <- M.lookup i . envDefs <$> get
  maybeToError Undef t

lookupFunTypeAndArgs :: Ident -> TypeChecker (Type, [Arg])
lookupFunTypeAndArgs i = typeAndArgs <$> lookupFun i
  where
    typeAndArgs def = case def of
      DefWiredIn (WiredIn t args _) -> (t, args)
      Def (FnDef t _ args _)        -> (t, args)

itemIdent :: Item -> Ident
itemIdent itm = case itm of
  NoInit i -> i
  Init i _ -> i

isNumeric :: Type -> Bool
isNumeric t = case t of
  { Int  -> True ; Doub -> True
  ; Bool -> False ; Void -> False ; Fun{} -> False
  }

typecheckItem :: Type -> Item -> TypeChecker ()
typecheckItem t i = case i of
  NoInit{} -> return ()
  Init _ e -> do
    t' <- infer e
    unless (t == t')
      $ throwError TypeMismatch

instance TypeCheck Expr where
instance Infer Expr where
  infer e = case e of
    EVar i -> lookupTypeVar i
    ELitInt{} -> return AST.Int
    ELitDoub{} -> return AST.Doub
    ELitTrue -> return AST.Bool
    ELitFalse -> return AST.Bool
    EApp i exprs -> do
      (t, args) <- lookupFunTypeAndArgs i
      argsMatch exprs args
      return t
    EString{} -> return AST.String
    Neg e0 -> do
      t <- infer e0
      unless (isNumeric t)
        $ throwError $ GenericError "Plz, can't negate non-numeric"
      return t
    Not e0 -> do
      t <- infer e0
      unless (t == AST.Bool)
        $ throwError $ GenericError "Plz, can't not non-boolean"
      return t
    EMul e0 _ e1 -> checkBinOp isNumeric e0 e1
    EAdd e0 _ e1 -> checkBinOp isNumeric e0 e1
    ERel e0 op e1 -> checkRel op e0 e1 >> return AST.Bool
    EAnd e0 e1 -> checkBinOp (== AST.Bool) e0 e1
    EOr e0 e1 -> checkBinOp (== AST.Bool) e0 e1

checkRel :: RelOp -> Expr -> Expr -> TypeChecker ()
checkRel _ e0 e1 = do
  t0 <- infer e0
  t1 <- infer e1
  unless (t0 == t1) throwMixErr

checkBinOp :: (Type -> Bool) -> Expr -> Expr -> TypeChecker Type
checkBinOp p e0 e1 = do
      t0 <- infer e0
      unless (p t0) throwNumErr
      t1 <- infer e1
      unless (p t1) throwNumErr
      unless (t1 == t0) throwMixErr
      return t1

throwNumErr :: TypeChecker a
throwNumErr = throwError $ GenericError "Expected numerical value"

throwMixErr :: TypeChecker a
throwMixErr = throwError $ GenericError "Plz, can't mix and match numerical values"

argsMatch :: [Expr] -> [Arg] -> TypeChecker ()
argsMatch = zipWithM_ argMatch

argMatch :: Expr -> Arg -> TypeChecker ()
argMatch e (Argument t _) = do
  t' <- infer e
  unless (t == t')
    $ throwError TypeMismatch
