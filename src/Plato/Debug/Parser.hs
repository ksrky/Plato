module Plato.Debug.Parser where

import Plato.Common.Info
import Plato.Syntax.Abstract

{-
reduceTopDecl :: TopDecl -> TopDecl
reduceTopDecl (DataDecl _ p1 p2 p3) = DataDecl dummyInfo p1 p2 p3'
 where
  p3' = map (\(p4, p5) -> (p4, map reduceType p5)) p3
reduceTopDecl (TypeDecl _ p1 p2 p3) = TypeDecl dummyInfo p1 p2 (reduceType p3)
reduceTopDecl (Decl decl) = Decl (reduceDecl decl)

reduceDecl :: Decl -> Decl
reduceDecl (FuncDecl _ p1 p2) = FuncDecl dummyInfo p1 (reduceExpr p2)
reduceDecl (FuncTyDecl _ p1 p2) = FuncTyDecl dummyInfo p1 (reduceType p2)

reduceType :: Type -> Type
reduceType (ConType _ p1) = ConType dummyInfo p1
reduceType (VarType _ p1) = VarType dummyInfo p1
reduceType (AppType p1 p2) = AppType (reduceType p1) (reduceType p2)
reduceType (ArrType _ p1 p2) = ArrType dummyInfo (reduceType p1) (reduceType p2)
reduceType (AllType _ p1 p2) = AllType dummyInfo p1 (reduceType p2)

reduceExpr :: Expr -> Expr
reduceExpr (VarExpr _ p1 p2) = VarExpr dummyInfo p1 (map reduceExpr p2)
reduceExpr (ConExpr _ p1 p2) = ConExpr dummyInfo p1 (map reduceExpr p2)
reduceExpr (LamExpr _ p1 p2) = LamExpr dummyInfo p1 (reduceExpr p2)
reduceExpr (LetExpr _ p1 p2) = LetExpr dummyInfo (map reduceDecl p1) (reduceExpr p2)
reduceExpr (CaseExpr _ p1 p2) = CaseExpr dummyInfo (reduceExpr p1) p2'
 where
  p2' = map (\(p3, p4, _) -> (reduceExpr p3, reduceExpr p4, dummyInfo)) p2
reduceExpr exp = exp
-}