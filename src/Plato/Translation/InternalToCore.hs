{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Translation.InternalToCore where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Internal.Syntax

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
        traexpr (TAppExpr fi e1 t2) = do
                t1 <- traexpr e1
                tyT2 <- transType ctx t2
                return $ TmTApp fi t1 tyT2
        traexpr (FloatExpr fi f) = return $ TmFloat fi f
        traexpr (StringExpr fi s) = return $ TmString fi s
        traexpr (LamExpr fi tyX e) | nameSpace tyX == TyVarName = case restty of
                AllType _ tyX ty -> do
                        let knK1 = KnStar -- tmp
                        ctx' <- addname fi tyX ctx
                        t2 <- transExpr ctx' ty e
                        return $ TmTAbs fi tyX knK1 t2
                _ -> throwError fi $ "Expected all type, but got " ++ show restty
        traexpr (LamExpr fi x e) = case restty of
                ArrType _ ty1 ty2 -> do
                        tyT1 <- transType ctx ty1
                        ctx' <- addname fi x ctx
                        t2 <- transExpr ctx' ty2 e
                        return $ TmAbs fi x tyT1 t2
                _ -> throwError fi $ "Expected arrow type, but got " ++ show restty
        traexpr (LetExpr fi d e1) = case d of
                FuncDecl fi2 f e2 ty -> do
                        tyT <- transType ctx ty
                        ctx' <- addname fi2 f ctx
                        t1 <- transExpr ctx' ty e2
                        let r = TmFix dummyInfo (TmAbs dummyInfo f tyT t1)
                        t2 <- transExpr ctx' restty e1
                        return $ TmLet fi dummyVarName r t2
                _ -> unreachable "Type declaration in let binding"
        traexpr (TagExpr fi l as) = do
                tyT <- transType ctx restty
                as' <- mapM traexpr as
                return $ TmTag fi l as' tyT
        traexpr (ProjExpr fi e l) = do
                t1 <- traexpr e
                return $ TmProj fi t1 l
        traexpr (RecordExpr fi fields) = do
                fields' <- forM fields $ \(li, ei) -> do
                        ti <- traexpr ei
                        return (li, ti)
                return $ TmRecord fi fields'
        traexpr (CaseExpr fi e alts) = do
                t <- traexpr e
                alts' <- forM alts $ \(pat, body) -> case pat of
                        ConPat fi1 li ps -> do
                                ctx' <- (`execStateT` ctx) $
                                        forM_ ps $ \(AnyPat fi2 mx) -> StateT $ \ctx -> do
                                                let x = fromMaybe dummyVarName mx
                                                ctx' <- addname fi1 x ctx
                                                return ((), ctx')
                                ti <- transExpr ctx' restty body
                                return (li, (length ps, ti))
                        AnyPat fi1 (Just x) -> do
                                ctx' <- addname fi1 x ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                        AnyPat fi1 Nothing -> do
                                ctx' <- addname fi1 dummyVarName ctx
                                ti <- traexpr body
                                return (str2varName "", (1, ti))
                return $ TmCase fi t alts'

transType :: MonadThrow m => Context -> Type -> m Ty
transType ctx = tratype
    where
        tratype :: MonadThrow m => Type -> m Ty
        tratype (VarType fi x) = do
                i <- getVarIndex fi ctx x
                return $ TyVar fi i (length ctx)
        tratype (ArrType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyArr fi ty1' ty2'
        tratype (AllType fi x ty) = do
                let knK1 = KnStar -- tmp: forall x.t = forall x::*.t
                ctx' <- addname fi x ctx
                ty' <- transType ctx' ty
                return $ TyAll fi x knK1 ty'
        tratype (AbsType fi x ty) = do
                let knK1 = KnStar -- tmp: \x.t = \x:*.t
                ctx' <- addname fi x ctx
                ty' <- transType ctx' ty
                return $ TyAll fi x knK1 ty'
        tratype (AppType fi ty1 ty2) = do
                ty1' <- tratype ty1
                ty2' <- tratype ty2
                return $ TyApp fi ty1' ty2'
        tratype (RecordType fi fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> do
                        field' <- tratype field
                        return (l, field')
                return $ TyRecord fi fields'
        tratype (SumType fieldtys) = do
                fields' <- forM fieldtys $ \(_, l, field) -> do
                        field' <- mapM tratype field
                        return (l, field')
                return $ TyVariant fields'

entryPoint :: Name
entryPoint = str2varName "main"

transDecl :: MonadThrow m => Decl -> StateT Context m Command
transDecl decl = StateT $ \ctx -> case decl of
        TypeDecl fi name ty -> do
                tyT <- transType ctx ty
                let ctx' = addbinding_ fi name (TyAbbBind tyT Nothing) ctx
                return (Bind name (TyAbbBind tyT Nothing), ctx')
        (FuncDecl fi f e ty) | f == entryPoint -> do
                t <- transExpr ctx ty e
                return (Eval t, ctx)
        FuncDecl fi f e ty -> do
                t <- transExpr ctx ty e
                tyT <- transType ctx ty
                let ctx' = addbinding_ fi f (VarBind tyT) ctx
                return (Bind f (TmAbbBind t (Just tyT)), ctx')

registerDecl :: MonadThrow m => Decl -> StateT Context m Command
registerDecl decl = StateT $ \ctx -> case decl of
        TypeDecl fi name ty -> do
                ctx' <- addname fi name ctx
                return (Bind name NameBind, ctx')
        FuncDecl fi f e ty -> do
                ctx' <- addname fi f ctx
                return (Bind f NameBind, ctx')

internal2core :: MonadThrow m => Context -> Decls -> m [Command]
internal2core ctx (Decls mns decls body) = (`evalStateT` ctx) $ do
        let imps = map Import mns
        cmds1 <- mapM registerDecl decls
        cmds2 <- mapM transDecl decls
        return $ imps ++ cmds1 ++ cmds2
