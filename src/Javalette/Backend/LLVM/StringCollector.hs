module Javalette.Backend.LLVM.StringCollector ( collect ) where

import Javalette.Backend.LLVM.Errors
import Javalette.Syntax as Jlt

collect :: Jlt.TopDef -> [String]
collect = collectStringsTopDef

collectStringsTopDef :: Jlt.TopDef -> [String]
collectStringsTopDef (Jlt.FnDef _ _ _ b) = collectStringsBlk b

collectStringsBlk :: Jlt.Blk -> [String]
collectStringsBlk (Jlt.Block stmts) = concatMap collectStringsStmt stmts

collectStringsStmt :: Jlt.Stmt -> [String]
collectStringsStmt s = case s of
  Jlt.Empty -> []
  Jlt.BStmt b -> collectStringsBlk b
  Jlt.Decl _ its -> concatMap collectStringsItem its
  Jlt.Ass _ e -> collectStringsExpr e
  Jlt.Incr{} -> impossibleRemoved ; Jlt.Decr{} -> impossibleRemoved
  Jlt.Ret e -> collectStringsExpr e
  Jlt.VRet -> []
  Jlt.Cond e s0 -> collectStringsExpr e ++ collectStringsStmt s0
  Jlt.CondElse e s0 s1 -> concat
    [ collectStringsExpr e
    , collectStringsStmt s0
    , collectStringsStmt s1
    ]
  Jlt.While e s0 -> collectStringsExpr e ++ collectStringsStmt s0
  Jlt.SExp e -> collectStringsExpr e
  Jlt.For{} -> impossibleRemoved

collectStringsItem :: Jlt.Item -> [String]
collectStringsItem i = case i of
  Jlt.NoInit{} -> []
  Jlt.Init _ e -> collectStringsExpr e
  Jlt.InitObj _ c -> collectStringsCons c

collectStringsCons :: Jlt.Constructor -> [String]
collectStringsCons c = case c of
  Jlt.ArrayCon _ e -> collectStringsExpr e

collectStringsExpr :: Jlt.Expr -> [String]
collectStringsExpr e = case e of
  Jlt.EVar{} -> [] ; Jlt.ELitInt{} -> []; Jlt.ELitDoub{} -> []
  Jlt.ELitTrue -> []; Jlt.ELitFalse -> []; Jlt.EApp _ es -> concatMap collectStringsExpr es
  Jlt.EString s -> pure s; Jlt.Neg e0 -> collectStringsExpr e0
  Jlt.Not e0 -> collectStringsExpr e0;
  Jlt.EMul e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAdd e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.ERel e0 _ e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAnd e0 e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EOr e0 e1 -> collectStringsExpr e0 ++ collectStringsExpr e1
  Jlt.EAnn _ e0 -> collectStringsExpr e0
  Jlt.Dot e0 _ -> collectStringsExpr e0
  Jlt.EIndex e0 idx -> collectStringsExpr e0 ++ collectStringsIndex idx

collectStringsIndex :: Jlt.Index -> [String]
collectStringsIndex (Jlt.Indx e) = collectStringsExpr e
