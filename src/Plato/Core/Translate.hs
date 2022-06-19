module Plato.Core.Translate where

import Plato.Core.Syntax
import Plato.Syntax.AST

import Control.Monad.State (MonadState (get), State)
import Data.List (elemIndex)
import GHC.Float (int2Float)

transExpr :: Expr -> State Context Term
transExpr (VarExpr x p) = do
        ctx <- get
        return $ TmVar (getVarIndex x ctx) (length ctx)
transExpr (IntExpr i) = do
        return $ TmFloat i
transExpr (StringExpr s) = return $ TmString s
transExpr (CallExpr x as p) = do
        v <- transExpr (VarExpr x p)
        app v as
    where
        app t [] = return t
        app t (a : as) = do
                a' <- transExpr a
                app (TmApp t a') as
transExpr (LamExpr x e p) = do
        x' <- pickfreshname x
        e' <- transExpr e
        return $ TmAbs x' undefined e'
transExpr (LetExpr ds e p) = undefined
transExpr (CaseExpr e pats p) = undefined

transType :: Type -> State Context Ty
transType (ConType x p) = return $ TyCon x
transType (VarType x p) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
transType (AppType ty1 ty2) = undefined
transType (FunType ty1 ty2 p) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyArr ty1' ty2'

getVarIndex :: String -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: " ++ var

transDecl :: Decl -> State Context (Name, Term)
transDecl (FuncDecl n e p) = do
        e' <- transExpr e
        return (n, e')
transDecl (FuncTyDecl n t p) = undefined

transTopDecl :: TopDecl -> State Context (Name, Term)
transTopDecl (DataDecl n params constrs p) = undefined
transTopDecl (TypeDecl n params ty p) = undefined
transTopDecl (Decl d) = transDecl d
