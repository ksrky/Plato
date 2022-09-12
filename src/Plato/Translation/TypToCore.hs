{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Translation.TypToCore where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Core.Command as C
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Translation.KindInfer
import Plato.Typing.Syntax
import Plato.Typing.Utils

import Control.Exception.Safe
import Control.Monad.State
import Data.Maybe (fromMaybe)

transExpr :: MonadThrow m => Context -> Type -> Expr -> m Term
transExpr ctx restty = traexpr
    where
        traexpr :: MonadThrow m => Expr -> m Term
        traexpr (VarExpr fi x) = do
                i <- getVarIndex fi ctx x
                return $ TmVar fi i (length ctx)
        traexpr (AppExpr fi e1 e2) = do
                t1 <- traexpr e1
                t2 <- traexpr e2
                return $ TmApp fi t1 t2
        traexpr (TAppExpr fi e1 ty2) = do
                t1 <- traexpr e1
                let s = getSubst ctx ty2
                tyT2 <- transType ctx s ty2
                return $ TmTApp fi t1 tyT2
        traexpr exp@(LamExpr fi x e) | nameSpace x == TyVarName = case restty of
                ty'@(AllType _ tyX ty) -> do
                        let knK1 = fromMaybe KnStar (lookup x $ getSubst ctx ty')
                        ctx' <- addName fi tyX ctx
                        t2 <- transExpr ctx' ty e
                        return $ TmTAbs fi x knK1 t2
                _ -> throwError fi $ "Expected all type, but got " ++ pretty restty
        traexpr e@(LamExpr fi x e1) = case restty of
                ArrType _ ty1 ty2 -> do
                        let s = getSubst ctx ty2
                        tyT1 <- transType ctx s ty1
                        ctx' <- addName fi x ctx
                        t2 <- transExpr ctx' ty2 e1
                        return $ TmAbs fi x tyT1 t2
                ty'@(AllType _ x ty) -> do
                        let knK1 = fromMaybe KnStar (lookup x $ getSubst ctx ty')
                        ctx' <- addBinding fi x (TyVarBind knK1) ctx
                        t2 <- transExpr ctx' ty e
                        return $ TmTAbs fi x knK1 t2
                _ -> throwError fi $ "Expected arrow type, but got " ++ pretty restty ++ "\n" ++ show e
        traexpr (LetExpr fi d e1) = case d of
                FuncDecl fi2 f e2 ty -> do
                        let s = getSubst ctx ty
                        tyT <- transType ctx s ty
                        ctx' <- addName fi2 f ctx
                        t1 <- transExpr ctx' ty e2
                        let r = TmFix dummyInfo (TmAbs dummyInfo f tyT t1)
                        t2 <- transExpr ctx' restty e1
                        return $ TmLet fi f r t2
                _ -> unreachable "Type declaration in let binding"
        traexpr (TagExpr fi l as) = do
                let s = getSubst ctx restty
                tyT <- transType ctx s restty
                as' <- mapM traexpr as
                return $ TmTag fi l as' tyT
        traexpr (ProjExpr fi e l) = do
                t1 <- traexpr e
                return $ TmProj fi t1 l
        traexpr (RecordExpr fi fields) = do
                fields' <- forM fields $ \(li, ei) -> case restty of
                        RecordType _ fieldtys -> case lookup li fieldtys of
                                Just tyi -> do
                                        ti <- transExpr ctx tyi ei
                                        return (li, ti)
                                _ -> throwError fi $ "label " ++ show li ++ " not found in the type signature"
                        _ -> throwError fi $ "Expected record type, but got " ++ show restty
                return $ TmRecord fi fields'
        traexpr (CaseExpr fi e alts) = do
                t1 <- traexpr e
                alts' <- forM alts $ \(pat, body) -> case pat of
                        ConPat fi1 li ps -> do
                                ctx' <- (`execStateT` ctx) $
                                        forM_ ps $ \(AnyPat fi2 mx) -> StateT $ \ctx -> do
                                                let x = fromMaybe dummyVarName mx
                                                ctx' <- addName fi1 x ctx
                                                return ((), ctx')
                                ti <- transExpr ctx' restty body
                                return (li, (length ps, ti))
                        AnyPat fi1 (Just x) -> do
                                ctx' <- addName fi1 x ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                        AnyPat fi1 Nothing -> do
                                ctx' <- addName fi1 dummyVarName ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                return $ TmCase fi t1 alts'

transType :: MonadThrow m => Context -> Subst -> Type -> m Ty
transType ctx s = tratype
    where
        tratype :: MonadThrow m => Type -> m Ty
        tratype (VarType fi x) | not (isVarExist ctx x) = return $ TyId fi x
        tratype (VarType fi x) = do
                i <- getVarIndex fi ctx x
                return $ TyVar fi i (length ctx)
        tratype (ArrType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyArr fi ty1' ty2'
        tratype (AllType fi x ty) = do
                let knK1 = fromMaybe KnStar (lookup x s) -- old: KnStar
                ctx' <- addBinding fi x (TyVarBind knK1) ctx
                tyT2 <- transType ctx' s ty
                return $ TyAll fi x knK1 tyT2
        tratype (AbsType fi x ty) = do
                let knK1 = fromMaybe KnStar (lookup x s) -- old: KnStar
                ctx' <- addBinding fi x (TyVarBind knK1) ctx
                tyT2 <- transType ctx' s ty
                return $ TyAll fi x knK1 tyT2
        tratype (AppType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyApp fi ty1' ty2'
        tratype (RecType fi x ty) = do
                let knK1 = fromMaybe KnStar (lookup x s) -- old: KnStar
                ctx' <- addBinding fi x (TyVarBind knK1) ctx
                tyT2 <- transType ctx' s ty
                return $ TyRec fi x knK1 tyT2
        tratype (RecordType fi fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> do
                        field' <- tratype field
                        return (l, field')
                return $ TyRecord fi fields'
        tratype (SumType fieldtys) = do
                fields' <- forM fieldtys $ \(_, l, field) -> do
                        field' <- mapM tratype field
                        return (l, field')
                return $ TyVariant dummyInfo fields' -- tmp: dummyInfo

transDecl :: MonadThrow m => Decl -> StateT Context m (Name, Binding)
transDecl decl = StateT $ \ctx -> case decl of
        TypeDecl fi name ty -> do
                let s = getSubst ctx ty
                    knK = kindInfer ctx ty
                tyT <- transType ctx s ty
                ctx' <- addBinding fi name (TyVarBind knK) ctx
                return ((name, TyAbbBind tyT Nothing), ctx')
        VarDecl fi f ty -> do
                let s = getSubst ctx ty
                tyT <- transType ctx s ty
                ctx' <- addName fi f ctx
                return ((f, VarBind tyT), ctx')
        FuncDecl fi f e ty -> do
                t <- transExpr ctx ty e
                let s = getSubst ctx ty
                tyT <- transType ctx s ty
                ctx' <- addName fi f ctx
                return ((f, TmAbbBind t (Just tyT)), ctx')

typ2core :: MonadThrow m => Context -> Decls -> m Commands
typ2core ctx (Decls modns decls (body, bodyty)) = (`evalStateT` ctx) $ do
        binds <- mapM transDecl decls
        ctx' <- get
        main <- transExpr ctx' bodyty body
        mainty <- transType ctx' bodyty
        return $ Commands{C.imports = modns, C.binds = zip (map getInfo decls) binds, C.body = (main, mainty)}
