module Plato.Core.Translate where

import Plato.Common.Name
import Plato.Core.Syntax
import Plato.Syntax.AST

import Control.Monad.State
import Control.Monad.Writer
import Data.List (elemIndex)
import GHC.Float (int2Float)
import qualified Plato.Common.Name as N

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
transExpr (LetExpr ds e p) = do
        e' <- transExpr e
        mkLet e' ds
    where
        mkLet t (FuncDecl x e p : ds) = do
                e' <- transExpr e
                mkLet (TmLet x e' t) ds
        mkLet t _ = return t
transExpr (CaseExpr e pats p) = do
        e' <- transExpr e
        pats' <- forM pats $ \(pat, body, pos) -> do
                pat' <- transExpr pat
                body' <- transExpr body
                return (pat', body')
        return $ TmCase e' pats'

transType :: Type -> State Context Ty
transType (ConType x p) = return $ TyCon x
transType (VarType x p) = do
        ctx <- get
        return $ TyVar (getVarIndex x ctx) (length ctx)
transType (AppType ty1 ty2) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyApp ty1' ty2'
transType (FunType ty1 ty2 p) = do
        ty1' <- transType ty1
        ty2' <- transType ty2
        return $ TyArr ty1' ty2'
transType (AllType x ty) = do
        ty' <- transType ty
        return $ TyAll x ty'

getVarIndex :: N.Name -> Context -> Int
getVarIndex var ctx = case elemIndex var (map fst ctx) of
        Just i -> i
        Nothing -> error $ "Unbound variable name: " ++ name2str var

transDecl :: TopDecl -> WriterT [(Name, Term)] (State Context) ()
transDecl (Decl (FuncDecl n e p)) = do
        e' <- lift $ transExpr e
        tell [(n, e')]
transDecl _ = return ()

transTopDecl :: TopDecl -> WriterT [(Name, Ty)] (State Context) ()
transTopDecl (DataDecl n params constrs p) = do
        constrs' <- forM constrs $ \(con, field) -> do
                field' <- lift $ mapM transType field
                let arr f = if null f then undefined {-self-} else TyArr (head f) (arr $ tail f)
                return (con, arr field')
        ty <- lift $ abstract (TyVariant constrs') params
        tell [(n, ty)]
transTopDecl (TypeDecl n params t p) = do
        ty <- lift $ transType t
        ty' <- lift $ abstract ty params
        tell [(n, ty')]
transTopDecl (Decl (FuncTyDecl n t p)) = do
        t' <- lift $ transType t
        tell [(n, t')]
transTopDecl _ = return ()

abstract :: Ty -> [Name] -> State Context Ty
abstract ty [] = return ty
abstract ty (p : ps) = do
        p' <- pickfreshname p
        TyAll p' <$> abstract ty ps

transProgram :: [TopDecl] -> State Context [(Name, Term)]
transProgram tds = do
        x <- execWriterT $ mapM_ transTopDecl tds
        y <- execWriterT $ mapM_ transDecl tds
        return y
