module Javalette.Backend.LLVM.Renamer (rename) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Javalette.Syntax as Jlt

import Debug.Trace

-- * Exports
rename :: Prog -> Prog
rename = rProg

-- * Types
type Variables = [Map Ident Ident]

data RS = RS
  { _fresh :: Int
  , _vars  :: Variables
  }

emptyRS :: RS
emptyRS = RS 0 []

type R a = State RS a

putFresh :: Int -> R ()
putFresh f = modify (\s -> s { _fresh = f})

mkFresh :: R Int
mkFresh = do
  i <- gets _fresh
  putFresh (succ i)
  return i

getVars :: R Variables
getVars = gets _vars

putVars :: Variables -> R ()
putVars vs = modify (\s -> s { _vars = vs })

lookupFirst :: Ident -> Variables -> Maybe Ident
lookupFirst _ [] = Nothing
lookupFirst i (m:ms) = case M.lookup i m of
  Nothing -> lookupFirst i ms
  x@Just{} -> x

-- * Helpers
newIdent :: R Ident
newIdent = intToIdent <$> mkFresh
  where
    intToIdent = Ident . (:) 'v' . show

modifyVars :: (Variables -> Variables) -> R ()
modifyVars f = do
  vars <- getVars
  putVars (f vars)

mapIdent :: Ident -> Ident -> R ()
mapIdent a b = modifyVars go
  where
    go [] = error "No scope"
    go (c:cs) = M.insert a b c : cs

-- TODO Maybe we wanna save part of the original name.
renameIdent :: Ident -> R Ident
renameIdent a = do
  b <- newIdent
  mapIdent a b
  return b

lookupIdent :: Ident -> R (Maybe Ident)
lookupIdent idnt = traceShowId idnt `seq` (lookupFirst idnt . traceShowId <$> getVars)

lookupIdentErr :: Ident -> R Ident
lookupIdentErr i
  = fromMaybe (error "Var not found - typechecker broken")
  <$> lookupIdent i

pushScope :: R ()
pushScope = modifyVars push
  where
    push = (:) M.empty

popScope :: R ()
popScope = modifyVars pop
  where
    pop = tail

withNewScope :: R a -> R a
withNewScope act = do { pushScope ; a <- act ; popScope ; return a }

-- * Translation
rProg :: Prog -> Prog
rProg (Program defs) = Program (map rTopDef defs)

rTopDef :: TopDef -> TopDef
rTopDef = (`evalState` emptyRS) . rTopDefM

rTopDefM :: TopDef -> R TopDef
rTopDefM (FnDef t i args blk) = withNewScope $ do
  args' <- mapM rArgM args
  blk'  <- rBlkM blk
  return (FnDef t i args' blk')

rArgM :: Arg -> R Arg
rArgM (Argument t i) = Argument t <$> renameIdent i

rBlkM :: Blk -> R Blk
rBlkM (Block stmts) = Block <$> withNewScope (mapM rStmtM stmts)

rStmtM :: Stmt -> R Stmt
rStmtM s = case s of
  Empty -> return Empty
  BStmt blk -> BStmt <$> rBlkM blk
  Decl t is -> Decl t <$> mapM rItemM is
  Ass i e -> Ass <$> lookupIdentErr i <*> rExprM e
  Incr i -> Incr <$> lookupIdentErr i
  Decr i -> Incr <$> lookupIdentErr i
  Ret e -> Ret <$> rExprM e
  VRet -> pure VRet
  Cond e s0 -> Cond <$> rExprM e <*> rStmtM s0
  CondElse e s0 s1 -> CondElse <$> rExprM e <*> rStmtM s0 <*> rStmtM s1
  While e s0 -> While <$> rExprM e <*> rStmtM s0
  SExp e -> SExp <$> rExprM e

rItemM :: Item -> R Item
rItemM itm = traceShowId <$> case trace "renaming" (traceShowId itm) of
  NoInit i -> NoInit <$> renameIdent i
  Init i e -> Init <$> renameIdent i <*> rExprM e

rExprM :: Expr -> R Expr
rExprM e = case e of
  EVar i -> EVar <$> lookupIdentErr i
  ELitInt{} -> ret ; ELitDoub{} -> ret
  ELitTrue -> ret ; ELitFalse -> ret
  EApp i es -> fmap (EApp i) (mapM rExprM es)
  EString{} -> ret
  Neg e0 -> Neg <$> rExprM e0
  Not e0 -> Not <$> rExprM e0
  EMul e0 op e1 -> EMul <$> rExprM e0 <*> pure op <*> rExprM e1
  EAdd e0 op e1 -> EAdd <$> rExprM e0 <*> pure op <*> rExprM e1
  ERel e0 op e1 -> ERel <$> rExprM e0 <*> pure op <*> rExprM e1
  EAnd e0 e1 -> EAnd <$> rExprM e0 <*> rExprM e1
  EOr e0 e1 -> EOr <$> rExprM e0 <*> rExprM e1
  where
    ret = return e
