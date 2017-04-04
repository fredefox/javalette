module Javaletter.Interpreter
  ( interpret
  ) where

import Data.Maybe
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import qualified Javalette.Syntax.AbsJavalette as AST
import qualified Javalette.Interpreter.Program as Jlt

interpret :: AST.Prog -> IO ()
interpret = runInterpreter . interp

reportError :: IO (Either InterpreterError (Value, Env)) -> IO ()
reportError act = act >>= \x -> case x of
  Left err -> prettyPrint err
  Right{}  -> return ()

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . prettyShow

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

-- TODO Not sure `ValVoid` is a good candidate for the null-value.
nullValue :: Value
nullValue = ValVoid

data Env = Env
  { envDefs :: Definitions
  , envVars :: Variables
  }

type Definitions = Map AST.Ident Definition
type Variables   = [Map AST.Ident Value]
data Definition = DefWiredin WiredIn | Def AST.TopDef

initEnv :: Env
initEnv = Env
  { envVars = []
  , envDefs = wiredInDefs
  }

data WiredIn = WiredIn
  { wiredInType :: AST.Type
  , wiredInArgs :: [AST.Arg]
  , wiredInIdent :: AST.Ident
  }

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


data InterpreterError
  = Generic String

instance Pretty InterpreterError where
  pPrint (Generic s) = text "ERROR:" <+> text s

type Interpreter a = StateT Env (ExceptT InterpreterError Jlt.Program) a

class Interpretable a where
  interp :: a -> Interpreter Value

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

instance Interpretable AST.Prog where
  interp (AST.Program defs) = valVoid $ do
    modifyDefs (M.union defs')
    case M.lookup main defs' of
      Nothing -> throwError (Generic "No main function")
      Just DefWiredin{} -> throwError (Generic "The main function should not be a built-in")
      Just (Def m)  -> interp m
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

instance Interpretable AST.TopDef where
  interp (AST.FnDef _ _ args blk) = withNewScope $ do
    mapM_ ((`addBinding` nullValue) . argIdent) args
    fromMaybe ValVoid <$> interpReturnBlk blk

argIdent :: AST.Arg -> AST.Ident
argIdent (AST.Argument _ i) = i

lookupVar :: AST.Ident -> Interpreter (Maybe Value)
lookupVar i = firstMatch <$> getVars
  where
    firstMatch :: Variables -> Maybe Value
    firstMatch = foldr ((>>) . M.lookup i) Nothing

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
interpReturnBlk (AST.Block stmts)
    = withNewScope
    $ fmap firstJust . mapM interpReturn $ stmts

firstJust [] = Nothing
firstJust (x : xs) = case x of
  Nothing -> firstJust xs
  Just{} -> x

interpReturn :: AST.Stmt -> Interpreter (Maybe Value)
interpReturn s = case s of
    AST.Empty -> return Nothing
    AST.BStmt blk -> interpReturnBlk blk
    AST.Decl _ its -> do
      vals <- mapM identAndValue its
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
      then interpReturn s0 >> interpReturn s
      else return Nothing
    AST.SExp e -> valueOf e >> return Nothing

isTrue :: Value -> Bool
isTrue (ValBool True) = True
isTrue _ = False

identAndValue :: AST.Item -> Interpreter (AST.Ident, Value)
identAndValue itm = case itm of
  AST.NoInit i -> return (i, nullValue)
  AST.Init i e -> (,) i <$> valueOf e

assign :: AST.Ident -> AST.Expr -> Interpreter ()
assign i e = do
  v <- valueOf e
  setVariable i v

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
  modFirst i f vars

setVariable :: AST.Ident -> Value -> Interpreter ()
setVariable i v = modifyVariable i (const v)

-- | Modifies the closest bound variable and throws an error if it does not
-- exist.
modFirst :: AST.Ident -> (Value -> Value) -> Variables -> Interpreter ()
modFirst i f vars =
  case vars of
    [] -> throwError (Generic "Assignment to non-existant variable")
    (m : ms) -> case M.lookup i m of
      Nothing -> modFirst i f vars
      Just v  -> putVars $ M.insert i (f v) m : ms

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
    Just (DefWiredin (WiredIn _ _ (AST.Ident i))) -> case i of
      "printInt" -> printInt vs
      "printDouble" -> printDouble vs
      "printString" -> printString vs
      "readInt" -> readInt vs
      "readDouble" -> readDouble vs
      _ -> throwError (Generic "Unknown built-int")
    Just (Def (AST.FnDef _ i args blk)) -> withNewScope $ do
      addBindings (map ident args) vs
      fromMaybe ValVoid <$> interpReturnBlk blk

addBindings :: [AST.Ident] -> [Value] -> Interpreter ()
addBindings = zipWithM_ addBinding

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

opTimes = wrapOp (*) (*) undefined
opDiv = wrapOp div (/) undefined
opMod = wrapOp mod undefined undefined
opPlus = wrapOp (+) (+) undefined
opMinus = wrapOp (-) (-) undefined
opLTH = wrapCmp (<) (<)
opLE = wrapCmp (<=) (<=)
opGTH = wrapCmp (>) (>)
opGE = wrapCmp (>=) (>=)
opEQU = wrapCmp (==) (==)
opNE = wrapCmp (/=) (/=)
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
  -> Value -> Value -> Value
wrapCmp intOp _  (ValInt a)  (ValInt b)  = ValBool (a `intOp` b)
wrapCmp _ doubOp (ValDoub a) (ValDoub b) = ValBool (a `doubOp` b)
wrapCmp _ _ _ _ = undefined
