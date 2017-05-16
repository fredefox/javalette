module Javalette.Interpreter
  ( interpret
  ) where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M

import qualified Javalette.Syntax as AST
import qualified Javalette.Interpreter.Program as Jlt
import Javalette.PrettyPrint

interpret :: AST.Prog -> IO ()
interpret = runInterpreter . interpProg

reportError :: IO (Either InterpreterError (Value, Env)) -> IO ()
reportError act = act >>= \x -> case x of
  Left err -> prettyPrint err
  Right{}  -> return ()

runInterpreter
  :: Interpreter Value
  -> IO ()
runInterpreter
  = reportError
  . Jlt.interpret
  . runExceptT
  . (`runStateT` initEnv)

data Value
  = ValInt Integer
  | ValDoub Double
  | ValVoid
  | ValBool Bool
  | ValString String
  deriving (Show)

data Env = Env
  { envDefs :: Definitions
  , envVars :: Variables
  } deriving (Show)

type Definitions = Map AST.Ident Definition
type Variables   = [Map AST.Ident Value]
data Definition = DefWiredin WiredIn | Def AST.TopDef deriving (Show)

initEnv :: Env
initEnv = Env
  { envVars = []
  , envDefs = wiredInDefs
  }

data WiredIn = WiredIn
  { _wiredInType :: AST.Type
  , _wiredInArgs :: [AST.Arg]
  , _wiredInIdent :: AST.Ident
  } deriving (Show)

wiredInDefs :: Definitions
wiredInDefs = M.fromList
  [ "printInt"    |-> WiredIn AST.Void      [AST.Argument AST.Int    (AST.Ident "_")] (AST.Ident "printInt")
  , "printDouble" |-> WiredIn AST.Void      [AST.Argument AST.Doub   (AST.Ident "_")] (AST.Ident "printDouble")
  , "printString" |-> WiredIn AST.Void      [AST.Argument AST.String (AST.Ident "_")] (AST.Ident "printString")
  , "readInt"     |-> WiredIn AST.Int   []                                            (AST.Ident "readInt")
  , "readDouble"  |-> WiredIn AST.Doub  []                                            (AST.Ident "readDouble")
  ]
  where
    i |-> def = (AST.Ident i, DefWiredin def)


newtype InterpreterError
  = Generic String

instance Pretty InterpreterError where
  pPrint (Generic s) = text "ERROR:" <+> text s

type Interpreter a = StateT Env (ExceptT InterpreterError Jlt.Program) a

modifyDefs :: (Definitions -> Definitions) -> Interpreter ()
modifyDefs f = modify (\e -> e { envDefs = f (envDefs e) })

modifyVars :: (Variables -> Variables) -> Interpreter ()
modifyVars f = modify (\e -> e { envVars = f (envVars e) })

getVars :: Interpreter Variables
getVars = envVars <$> get

getDefs :: Interpreter Definitions
getDefs = envDefs <$> get

putVars :: Variables -> Interpreter ()
putVars vars = modify (\e -> e { envVars = vars })


interpProg :: AST.Prog -> Interpreter Value
interpProg (AST.Program defs) = valVoid $ do
  modifyDefs (M.union defs')
  case M.lookup main defs' of
    Nothing -> throwError (Generic "No main function")
    Just DefWiredin{} -> throwError (Generic "The main function should not be a built-in")
    Just (Def m)  -> interpTopDef m
  where
    defs' = toMap defs
    toMap :: [AST.TopDef] -> Definitions
    toMap = M.fromList . map toPair
    toPair :: AST.TopDef -> (AST.Ident, Definition)
    toPair def@(AST.FnDef _ i _ _) = (i, Def def)
    main :: AST.Ident
    main = AST.Ident "main"

pushScope, popScope :: Interpreter ()
pushScope = modify (\e -> e { envVars = M.empty : envVars e})
popScope = modify (\e -> e { envVars = tail (envVars e)})

withNewScope :: Interpreter a -> Interpreter a
withNewScope act = do
  pushScope
  a <- act
  popScope
  return a

interpTopDef :: AST.TopDef -> Interpreter Value
interpTopDef (AST.FnDef _ _ args blk) = withNewScope $ do
  mapM_ (\(AST.Argument t i)-> i `addBinding` defaultValue t) args
  fromMaybe ValVoid <$> interpReturnBlk blk

lookupVar :: AST.Ident -> Interpreter (Maybe Value)
lookupVar i = firstMatch i <$> getVars

firstMatch :: Ord k => k -> [Map k a] -> Maybe a
firstMatch i = firstJust . map (M.lookup i)

lookupDef :: AST.Ident -> Interpreter (Maybe Definition)
lookupDef i = M.lookup i <$> getDefs

-- | Adds a binding to the current scope and throws an error if it already exists
addBinding :: AST.Ident -> Value -> Interpreter ()
addBinding i val = do
  v <- getVars
  case v of
    []     -> throwError (Generic "No scope to bind variables to")
    m : ms -> case M.lookup i m of
      Just{} -> throwError (Generic "Variable already bound")
      Nothing -> putVars (M.insert i val m : ms)

interpReturnBlk :: AST.Blk -> Interpreter (Maybe Value)
interpReturnBlk (AST.Block stmts) = withNewScope $ untillReturn stmts

-- We cannot use `mapM` because `>>` is strict in `Interpreter`. We
-- wanna short-cut the computation at the time when we encounter the first
-- return statement (represented by a `Just{}`).
untillReturn :: [AST.Stmt] -> Interpreter (Maybe Value)
untillReturn []        = return Nothing
untillReturn (x : xs) = do
  r <- interpReturn x
  case r of
    Nothing -> untillReturn xs
    Just{}  -> return r

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x : xs) = case x of
  Nothing -> firstJust xs
  Just{} -> x

interpReturn :: AST.Stmt -> Interpreter (Maybe Value)
interpReturn s = case s of
    AST.Empty -> return Nothing
    AST.BStmt blk -> interpReturnBlk blk
    AST.Decl t its -> do
      vals <- mapM (identAndValue t) its
      mapM_ (uncurry addBinding) vals
      return Nothing
    AST.Ass i e -> assign i e >> return Nothing
    AST.Incr i  -> incr i >> return Nothing
    AST.Decr i  -> decr i >> return Nothing
    AST.Ret e   -> Just <$> valueOf e
    AST.VRet    -> return (Just ValVoid)
    AST.Cond e s0 -> do
      v <- valueOf e
      if isTrue v
      then interpReturn s0
      else return Nothing
    AST.CondElse e s0 s1 -> do
      v <- valueOf e
      if isTrue v
      then interpReturn s0
      else interpReturn s1
    AST.While e s0 -> do
      v <- valueOf e
      if isTrue v
      then do
        r <- interpReturn s0
        case r of
          Nothing -> interpReturn s
          Just{} -> return r
      else return Nothing
    AST.SExp e -> valueOf e >> return Nothing

isTrue :: Value -> Bool
isTrue (ValBool True) = True
isTrue _ = False

identAndValue :: AST.Type -> AST.Item -> Interpreter (AST.Ident, Value)
identAndValue t itm = case itm of
  AST.NoInit i -> return (i, defaultValue t)
  AST.Init i e -> (,) i <$> valueOf e

defaultValue :: AST.Type -> Value
defaultValue t = case t of
  AST.Int -> ValInt 0
  AST.Doub -> ValDoub 0
  AST.Bool -> ValBool False
  AST.Void -> ValVoid
  AST.String -> ValString ""
  AST.Fun{} -> error "Cannot assign functions to variables"

assign :: AST.Expr -> AST.Expr -> Interpreter ()
assign lv e = case lv of
  AST.EVar i -> do
    v <- valueOf e
    setVariable i v
  _ -> error "Not implemented"

incr, decr :: AST.Ident -> Interpreter ()
incr = (`modifyVariable` incrVal)
decr = (`modifyVariable` decrVal)

incrVal, decrVal :: Value -> Value
incrVal (ValInt i) = ValInt (succ i)
incrVal (ValDoub d) = ValDoub (succ d)
incrVal (ValBool b) = ValBool (succ b)
incrVal _ = error "Cannot increment non-numeric value"
decrVal (ValInt i) = ValInt (pred i)
decrVal (ValDoub d) = ValDoub (pred d)
decrVal (ValBool b) = ValBool (pred b)
decrVal _ = error "Cannot decrement non-numeric value"

modifyVariable :: AST.Ident -> (Value -> Value) -> Interpreter ()
modifyVariable i f = do
  vars <- getVars
  case modFirst i f vars of
    Nothing -> throwError (Generic "Assignment to non-existant variable")
    Just v  -> putVars v

setVariable :: AST.Ident -> Value -> Interpreter ()
setVariable i v = modifyVariable i (const v)

-- | Modifies the closest bound variable and throws an error if it does not
-- exist.
modFirst :: AST.Ident -> (Value -> Value) -> Variables -> Maybe Variables
modFirst i f vars =
  case vars of
    [] -> Nothing
    (m : ms) -> case M.lookup i m of
      Nothing -> (m :) <$> modFirst i f ms
      Just v  -> Just $ M.insert i (f v) m : ms

valueOf :: AST.Expr -> Interpreter Value
valueOf e = case e of
  AST.EVar i -> do
    v <- lookupVar i
    case v of
      Nothing -> throwError (Generic "Reading unbound variable")
      Just a  -> return a
  AST.ELitInt i   -> return (ValInt i)
  AST.ELitDoub d  -> return (ValDoub d)
  AST.ELitTrue    -> return (ValBool True)
  AST.ELitFalse   -> return (ValBool False)
  AST.EString s   -> return (ValString s)
  AST.EApp i es   -> apply i es
  AST.Neg e0 -> valNeg <$> valueOf e0
  AST.Not e0 -> valNot <$> valueOf e0
  AST.EMul e0 op e1 -> liftM2 (unMul op) (valueOf e0) (valueOf e1)
  AST.EAdd e0 op e1 -> liftM2 (unAdd op) (valueOf e0) (valueOf e1)
  AST.ERel e0 op e1 -> liftM2 (unRel op) (valueOf e0) (valueOf e1)
  AST.EAnd e0 e1    -> liftM2 opAnd      (valueOf e0) (valueOf e1)
  AST.EOr  e0 e1    -> liftM2 opOr       (valueOf e0) (valueOf e1)
  where
    valNot (ValBool b) = ValBool (not b)
     -- Really this is can only happen if there is a type-error.
    valNot v           = v
    valNeg (ValInt i)  = ValInt  (negate i)
    valNeg (ValDoub d) = ValDoub (negate d)
    -- Really this is a type-error.
    valNeg v           = v
    unMul AST.Times = opTimes
    unMul AST.Div   = opDiv
    unMul AST.Mod   = opMod
    unAdd AST.Plus  = opPlus
    unAdd AST.Minus = opMinus
    unRel AST.LTH   = opLTH
    unRel AST.LE    = opLE
    unRel AST.GTH   = opGTH
    unRel AST.GE    = opGE
    unRel AST.EQU   = opEQU
    unRel AST.NE    = opNE

apply :: AST.Ident -> [AST.Expr] -> Interpreter Value
apply i es = do
  f <- lookupDef i
  vs <- values es
  case f of
    Nothing -> throwError (Generic "Reference to undefined function")
    Just (DefWiredin (WiredIn _ _ (AST.Ident n))) -> case n of
      "printInt" -> printInt vs
      "printDouble" -> printDouble vs
      "printString" -> printString vs
      "readInt" -> readInt vs
      "readDouble" -> readDouble vs
      _ -> throwError (Generic "Unknown built-int")
    Just (Def (AST.FnDef _ _ args blk)) -> withNewScope $ do
      addBindings (map ident args) vs
      fromMaybe ValVoid <$> interpReturnBlk blk

addBindings :: [AST.Ident] -> [Value] -> Interpreter ()
addBindings = zipWithM_ addBinding

ident :: AST.Arg -> AST.Ident
ident (AST.Argument _ i) = i

values :: [AST.Expr] -> Interpreter [Value]
values = mapM valueOf

printInt, printDouble, printString, readInt, readDouble :: [Value] -> Interpreter Value
printInt = intVal >=> valVoid . Jlt.printInt
printDouble = doubVal >=> valVoid . Jlt.printDouble
printString = stringVal >=> valVoid . Jlt.printString
readInt _ = ValInt <$> Jlt.readInt
readDouble _ = ValDoub <$> Jlt.readDouble

intVal :: [Value] -> Interpreter Integer
intVal [ValInt i] = return i
intVal _   = throwError (Generic "Argument mismatch")

doubVal :: [Value] -> Interpreter Double
doubVal [ValDoub d] = return d
doubVal _   = throwError (Generic "Argument mismatch")

stringVal :: [Value] -> Interpreter String
stringVal [ValString d] = return d
stringVal _   = throwError (Generic "Argument mismatch")

valVoid :: Interpreter a -> Interpreter Value
valVoid act = act >> return ValVoid

opTimes, opDiv, opMod, opPlus, opMinus, opLTH, opLE, opGTH, opGE,
  opEQU, opNE, opAnd, opOr :: Value -> Value -> Value

-- This can probably be made a lot prettier with type-families.
opTimes = wrapOp (*) (*) undefined
opDiv = wrapOp div (/) undefined
opMod = wrapOp mod undefined undefined
opPlus = wrapOp (+) (+) undefined
opMinus = wrapOp (-) (-) undefined
opLTH = wrapCmp (<) (<) (<)
opLE = wrapCmp (<=) (<=) (<=)
opGTH = wrapCmp (>) (>) (>)
opGE = wrapCmp (>=) (>=) (>=)
opEQU = wrapCmp (==) (==) (==)
opNE = wrapCmp (/=) (/=) (/=)
opAnd = wrapOp undefined undefined (&&)
opOr = wrapOp undefined undefined (||)

wrapOp
  :: (Integer -> Integer -> Integer)
  -> (Double  -> Double  -> Double)
  -> (Bool    -> Bool    -> Bool)
  -> Value -> Value -> Value
wrapOp intOp _  _ (ValInt a)  (ValInt b)  = ValInt (a `intOp` b)
wrapOp _ doubOp _ (ValDoub a) (ValDoub b) = ValDoub (a `doubOp` b)
wrapOp _ _ boolOp (ValBool a) (ValBool b) = ValBool (a `boolOp` b)
wrapOp _ _ _ _ _ = undefined

wrapCmp
  :: (Integer -> Integer -> Bool)
  -> (Double  -> Double  -> Bool)
  -> (Bool    -> Bool    -> Bool)
  -> Value -> Value -> Value
wrapCmp intOp _ _  (ValInt a)  (ValInt b)  = ValBool (a `intOp` b)
wrapCmp _ doubOp _ (ValDoub a) (ValDoub b) = ValBool (a `doubOp` b)
wrapCmp _ _ boolOp (ValBool a) (ValBool b) = ValBool (a `boolOp` b)
wrapCmp _ _ _ _ _ = undefined
