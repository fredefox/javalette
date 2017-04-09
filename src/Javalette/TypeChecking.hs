{- | Typechecking of the Javalette programming language -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Javalette.TypeChecking
  ( runTypeChecker
  , evalTypeChecker
  , TypeChecker()
  , TypeCheck(..)
  , TypeCheckingError(..)
  , Infer(..)
  , staticControlFlowCheck
  , typecheck
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State

import Javalette.Syntax.AbsJavalette
import qualified Javalette.Syntax.AbsJavalette as AST
import Javalette.PrettyPrint

-- | Performs typechecking of a progrma
typecheck :: Prog -> Either TypeCheckingError ()
typecheck p = evalTypeChecker $ do
  typechk p
  staticControlFlowCheck p

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

instance Pretty TypeCheckingError where
  pPrint err = case err of
    EmptyEnvironment -> text "Empty environment"
    Uninitialized -> text "Uninitialized variable"
    TypeMismatch -> text "Types mismatch"
    NoReturnStatement -> text
      "No return statement in function of non-void return type"
    Undef -> text "Undefined variable"
    GenericError s -> text "Generic error:" <+> text s

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
  -- TODO: Some elements of the AST need more arguments to be type-checked.
  -- So this type-class is sort of less than ideal.
  typechk :: a -> TypeChecker ()
  default typechk :: Infer a => a -> TypeChecker ()
  typechk = void . infer

-- | This class defines something which type can be inferred.
class TypeCheck a => Infer a where
  infer :: a -> TypeChecker Type

-- | The environment used during type-checking.
data Env = Env
  { envVars :: [Map Ident Type]
  , envDefs :: Definitions
  }

type Definitions = Map Ident Definition

-- | A definition is either part of the built-in methods or it is
-- part of the AST.
data Definition = DefWiredIn WiredIn | Def TopDef

-- | Modifies the variables - used during assignment.
modifyVars :: ([Map Ident Type] -> [Map Ident Type]) -> Env -> Env
modifyVars f e = e { envVars = f (envVars e) }

-- | Modifies the definitions - used when accumulating all locally defined
-- definitions.
modifyDefs :: (Definitions -> Definitions) -> Env -> Env
modifyDefs f e = e { envDefs = f (envDefs e) }

-- | The control-structure used for typechecking.
type TypeChecker a = StateT Env (Except TypeCheckingError) a

-- | Unwraps the layers of the typechecking monad.
runTypeChecker
  :: TypeChecker a
  -> Either TypeCheckingError (a, Env)
runTypeChecker t = runExcept . runStateT t $ initEnv

-- | Like `runTypeChecker` but discards the state of the type-checker.
evalTypeChecker :: TypeChecker a -> Either TypeCheckingError a
evalTypeChecker t = runExcept . evalStateT t $ initEnv

-- | The environment used during type-checking. Closures are implemented by
-- using a *list of* maps.
initEnv :: Env
initEnv = Env
  { envVars = []
  , envDefs = wiredInDefs
  }

-- NOTE Do we wanne refer to the types defined by the AST or do we want to
-- define our own similar varions?
-- | A `WiredIn` is similar to a `TopDef` from the Javalette AST, but it has no
-- implementation.
data WiredIn = WiredIn
  { _wiredInType :: Type
  , _wiredInArgs :: [Arg]
  , _wiredInIdent :: Ident
  }

-- | The list of wired-in definitions.
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

-- | Adds a new scope. Used when entering blocks.
newScope :: TypeChecker ()
newScope = modify (modifyVars $ \xs -> M.empty : xs)

-- | Used finding all top-level definitions.
unionDefs :: Definitions -> TypeChecker ()
unionDefs defs = modify (modifyDefs $ M.union defs)

-- | Adds a binding to the current scope. It's an error if you bind to an
-- already bound variable.
addBinding :: Ident -> Type -> TypeChecker ()
addBinding i t = do
  e <- get
  case envVars e of
    []      -> throwError EmptyEnvironment
    x : xs  -> do
      m <- insertMaybe i t x
      put $ e { envVars = m : xs }

-- | Inserts a value into a map if it doesn't already exist.
-- This function performs two lookups.
insertMaybe
  :: (Ord k, MonadError TypeCheckingError m)
  => k
  -> a
  -> Map k a
  -> m (Map k a)
insertMaybe k a m = case M.lookup k m of
  Nothing -> return $ M.insert k a m
  Just{}  -> throwError (GenericError "Bind to bound var")

-- | Pluralized version of `addBinding`.
addBindings :: [(Ident, Type)] -> TypeChecker ()
addBindings = mapM_ (uncurry addBinding)

-- | Adds arguments to the current scope.
addArgs :: [Arg] -> TypeChecker ()
addArgs = addBindings . map identAndType
  where
    identAndType (Argument t i) = (i, t)

-- | Creates the initial mapping between identifiers and locally defined
-- functions.
mkFunEnv :: [TopDef] -> Definitions
mkFunEnv = M.fromList . map (\def@(FnDef _ i _ _) -> (i, Def def))

instance TypeCheck Prog where
  typechk (Program defs)
    =  unionDefs (mkFunEnv defs)
    >> mapM_ typechk defs

-- | `typecheck` checks that all the return-statements in the functions definition has the
-- specified type. Note that this does *not* guarantee that all paths return a
-- value of the given type. For this you need `staticControlFlowCheck`.
instance TypeCheck TopDef where
  typechk (FnDef t _ args blk) = do
    _ <- newScope
    _ <- addArgs args
    typecheckBlk t blk

-- I don't like that type-checking does not have the same interface for all
-- parts of the language. A solution to this would be to expand state to also
-- carry around the "current expected type" but this is also not so elegant
-- because there are many situations where that will then not be needed.
-- instance TypeCheck Blk where
-- | Almost a `TypeCheck` instance for block-statements. See `typecheckStmts`.
typecheckBlk :: Type -> Blk -> TypeChecker ()
typecheckBlk t (Block stms) = mapM_ (typecheckStmt t) stms

-- Almost the `TypeCheck` instance for `Stmt`. We check that *if* there is a
-- return-statement, then the inferred type of that expression has the given
-- type.
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
  SExp e -> typechk e

-- | Helper function that checks that an expression has a boolean type.
inferBoolean :: Expr -> TypeChecker ()
inferBoolean e = do
  te <- infer e
  unless (te == AST.Bool)
    $ throwError TypeMismatch

-- | Converts a `Maybe` to an error in `MonadError`.
maybeToError :: MonadError e m => e -> Maybe a -> m a
maybeToError err Nothing  = throwError err
maybeToError _   (Just a) = return a

-- | Looks up the type of a variable in the current scope.
lookupTypeVar
  :: Ident -- ^ The name of a variable
  -> TypeChecker Type
lookupTypeVar i = do
  t <- firstMatch . envVars <$> get
  maybeToError Uninitialized t
    where
      firstMatch :: [Map Ident Type] -> Maybe Type
      firstMatch []       = Nothing
      firstMatch (m : ms) = case M.lookup i m of
        Nothing -> firstMatch ms
        Just t  -> Just t

-- | Looks up a definition.
lookupFun
  :: Ident -- ^ The name of a function
  -> TypeChecker Definition
lookupFun i = do
  t <- M.lookup i . envDefs <$> get
  maybeToError Undef t

-- | Looks up the type and arguments of a definitions.
lookupFunTypeAndArgs
  :: Ident -- ^ The name of a function
  -> TypeChecker (Type, [Arg])
lookupFunTypeAndArgs i = typeAndArgs <$> lookupFun i
  where
    typeAndArgs def = case def of
      DefWiredIn (WiredIn t args _) -> (t, args)
      Def (FnDef t _ args _)        -> (t, args)

-- | The identifier associated with an `Item`.
itemIdent :: Item -> Ident
itemIdent itm = case itm of
  NoInit i -> i
  Init i _ -> i

-- | Some types are "numerical" values.
isNumeric :: Type -> Bool
isNumeric t = case t of
  { Int  -> True ; Doub -> True
  ; Bool -> False ; Void -> False ; Fun{} -> False; String{} -> False
  }

-- | Almost the `TypeCheck` instance for a `Item`.
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

-- | Helper-function for typechecking `RelOp`.
checkRel :: RelOp -> Expr -> Expr -> TypeChecker ()
checkRel _ e0 e1 = do
  t0 <- infer e0
  t1 <- infer e1
  unless (t0 == t1) throwMixErr

-- | Helper-function for typechecking binary operators.
checkBinOp :: (Type -> Bool) -> Expr -> Expr -> TypeChecker Type
checkBinOp p e0 e1 = do
      t0 <- infer e0
      unless (p t0) throwNumErr
      t1 <- infer e1
      unless (p t1) throwNumErr
      unless (t1 == t0) throwMixErr
      return t1

-- | Generic numerical error.
throwNumErr :: TypeChecker a
throwNumErr = throwError $ GenericError "Expected numerical value"

-- | Generic error for mixing different numerical types.
throwMixErr :: TypeChecker a
throwMixErr = throwError $ GenericError "Plz, can't mix and match numerical values"

-- | Checks that the type of a list of expression match the list of arguments.
-- This also checks that the arity match.
argsMatch
  :: [Expr] -- ^ The expressions to infer the type of
  -> [Arg]  -- ^ the arguments whose types to match against
  -> TypeChecker ()
argsMatch = zipWithMRagged_ (GenericError "Arg mismatch") argMatch

-- | A version of `Control.Monad.zipWithM_` that rejects ragged lists with the
-- specified error.
zipWithMRagged_ :: MonadError e m => e -> (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithMRagged_ e _ [] ys = case ys of
  [] -> return ()
  _  -> throwError e
zipWithMRagged_ e f (x : xs) ys = case ys of
  []        -> throwError e
  (y : yss) -> f x y >> zipWithMRagged_ e f xs yss

-- | Checks that an expression matches the type of an argument.
argMatch :: Expr -> Arg -> TypeChecker ()
argMatch e (Argument t _) = do
  t' <- infer e
  unless (t == t')
    $ throwError TypeMismatch

------------------------------------------------------------
-- * Static control flow checks

------------------------------------------------------------
-- TODO: This section should be moved to its own file I feel.

-- | Checks that a function has return-statements in possible branches. Note
-- that this method is not complete. I.e. some functions that would always have
-- a return-statement may be rejected. E.g.:
--
-- > int main() {
-- >   if(entscheidung()) { return 0; }
-- > }
--
-- Where `entcheidung` is a function that always returns `true` but proving this
-- is undecidable.
staticControlFlowCheck :: Prog -> TypeChecker ()
staticControlFlowCheck (Program defs) = do
  _ <- unionDefs (mkFunEnv defs)
  mapM_ staticControlFlowCheckDef defs

staticControlFlowCheckDef
  :: TopDef -> TypeChecker ()
staticControlFlowCheckDef (FnDef t _ args blk) = do
  _ <- newScope
  _ <- addArgs args
  ft <- inferBlk blk
  case ft of
    Always t' -> when (t /= t')       (throwError (GenericError "Inferred type doesn't match expected type"))
    _         -> when (t /= AST.Void) (throwError (GenericError "Control-flow is out of whack you!"))

-- | Infers the return-type of a statement and return how "often" we see this
-- type. The frequency with which we see the type can be seen as wether or not
-- *all*, *some* or *none* of the possible paths return the given type.
inferStmt :: Stmt -> TypeChecker (Frequency Type)
inferStmt s = case s of
  Empty          -> return Never
  BStmt blk      -> newScope >> inferBlk blk
  Decl t0 its    -> do
    addBindings $ map (\i -> (itemIdent i, t0)) its
    return Never
  Ass i e        -> do
    assign i e
    return Never
  Incr i         -> incr i >> return Never
  Decr i         -> decr i >> return Never
  Ret e          -> Always <$> infer e
  VRet           -> return (Always AST.Void)
  Cond e s0      -> do
    v <- staticValue e
    case v of
      (Just (ValBool True))  -> inferStmt s0
      (Just (ValBool False)) -> return Never
      Just{}                 -> throwError (GenericError "Impossible type-error")
      Nothing      -> sometimes <$> inferStmt s0
  CondElse e s0 s1 -> do
    v <- staticValue e
    case v of
      (Just (ValBool True))  -> inferStmt s0
      (Just (ValBool False)) -> inferStmt s1
      (Just _)               -> throwError (GenericError "Impossible type-error")
      Nothing                -> liftM2 least (inferStmt s0) (inferStmt s1)
  While e s0 -> do
    v <- staticValue e
    case v of
      (Just (ValBool True))  -> do
        t <- inferStmt s0
        case t of
          Never -> return (Always AST.Void)
          _     -> return t
      (Just (ValBool False)) -> return Never
      (Just _)               -> throwError (GenericError "Impossible type-error")
      Nothing                -> sometimes <$> inferStmt s0
  SExp{} -> return Never

-- TODO Unimplemented.
assign :: Ident -> Expr -> TypeChecker ()
assign _ _ = return ()
incr, decr :: Ident -> TypeChecker ()
incr _ = return ()
decr _ = return ()

inferBlk :: Blk -> TypeChecker (Frequency Type)
inferBlk (Block stmts) = inferStmts stmts

-- | Inferring the *return-type* of a sequence of statements. With
-- value-inference this is not simply a fold or map across the list of
-- statements. Consider these two programs:
--
-- > int f(x) { if(true) return 0; }
--
-- > int f(x) { if(x) return 0; }
--
-- The first one is acceptable because it always returns an integer. Whereas the
-- latter may take a branch without a valid return-statement.
inferStmts :: [Stmt] -> TypeChecker (Frequency Type)
inferStmts stmts = case stmts of
  -- Note: We do not assume an empty return-statement.
  []     -> return Never
  s : ss -> do
    fts <- inferStmt s
    case fts of
      Always{}    -> return fts
      Sometimes{} -> inferStmts ss
      Never       -> inferStmts ss

data Frequency a = Always a | Sometimes a | Never deriving (Show)

-- | Demotes an `Always` to a `Sometimes`.
sometimes :: Frequency a -> Frequency a
sometimes f = case f of
  (Always a)  -> Sometimes a
  Sometimes{} -> f
  Never       -> f

-- | Returns the least frequent and selecting the first value if they are
-- equally frequent.
least :: Frequency a -> Frequency a -> Frequency a
least Never         _  = Never
least a@Sometimes{} fb = case fb of
  Never -> Never
  _     -> a
least a@Always{}    fb = case fb of
  Never       -> Never
  Sometimes{} -> fb
  _           -> a

-- TODO Combine with `Javalette.Interpreter.Value`.
data Value
    = ValInt Integer
    | ValDoub Double
    | ValBool Bool
    | ValString String
    deriving (Show)

-- | Tries to infer the static value of an expression. Returns `Nothing` if no
-- such guarantee can be made. The implementation is generous with the usage of
-- `Nothing` in these cases.
staticValue :: Expr -> TypeChecker (Maybe Value)
staticValue e = case e of
    -- TODO `lookupValVar` is not yet implemented!
    --     EVar i -> Just <$> lookupValVar i
    EVar{} -> stub
    ELitInt i -> return (Just (ValInt i))
    ELitDoub d -> return (Just (ValDoub d))
    ELitTrue -> return (Just (ValBool True))
    ELitFalse -> return (Just (ValBool False))
    EString s -> return (Just (ValString s))
    Neg e0 -> valNeg `changeVal` staticValue e0
    Not e0 -> valNot `changeVal` staticValue e0
    -- TODO We can also check static types in these cases.
    EMul{} -> stub
    EAdd{} -> stub
    ERel{} -> stub
    EAnd{} -> stub
    EOr{}  -> stub
    EApp{} -> stub
    where
      changeVal f m = fmap f <$> m
      valNot (ValBool b) = ValBool (not b)
      -- Really this is can only happen if there is a type-error.
      valNot v           = v
      valNeg (ValInt i)  = ValInt  (negate i)
      valNeg (ValDoub d) = ValDoub (negate d)
      -- Really this is a type-error.
      valNeg v           = v
      stub = return Nothing
