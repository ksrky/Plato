{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Transl.TypToCore where

import Plato.Core.Context

import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.Common.Name.Global

import Plato.Typing.Bundle
import Plato.Typing.KindInfer
import Plato.Typing.TcTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import Prettyprinter

transExpr :: (MonadThrow m, MonadIO m) => T.KnEnv -> Context -> T.Expr -> m C.Term
transExpr knenv ctx = traexpr
    where
        traexpr :: (MonadThrow m, MonadIO m) => T.Expr -> m C.Term
        traexpr (T.VarE x) = do
                i <- getVarIndex ctx x
                return $ C.TmVar i (length ctx)
        traexpr (T.AbsE x (Just ty1) e2) = do
                tyT1 <- transType ctx ty1
                let ctx' = addName (localName x) ctx
                t2 <- transExpr knenv ctx' e2
                return $ C.TmAbs (unLoc x) tyT1 t2
        traexpr (T.AppE e1 e2) = do
                t1 <- traexpr e1
                t2 <- traexpr e2
                return $ C.TmApp t1 t2
        traexpr (T.TAppE e1 tys) = do
                t1 <- traexpr e1
                tyTs' <- mapM (transType ctx) tys
                return $ foldl C.TmTApp t1 tyTs'
        traexpr (T.TAbsE xs e1) = do
                let ctx' = foldl (flip $ addName . localName) ctx xs
                t1 <- transExpr knenv ctx' e1
                return $ foldr (C.TmTAbs . unLoc) t1 xs
        traexpr (T.LetE [T.FuncD x e11 ty12] e2) = do
                ty12' <- checkKindStar knenv NoSpan ty12
                tyT1 <- transType ctx ty12'
                let ctx' = addName (localName x) ctx
                t1 <- transExpr knenv ctx' e11
                let t1' = C.TmFix (C.TmAbs (unLoc x) tyT1 t1)
                t2 <- transExpr knenv ctx' e2
                return $ C.TmLet (unLoc x) t1' t2
        traexpr (T.TagE l as ty) = do
                as' <- mapM traexpr as
                tyT <- transType ctx ty
                return $ C.TmTag (g_name l) as' tyT
        traexpr (T.FoldE ty1) = C.TmFold <$> transType ctx ty1
        traexpr (T.ProjE e1 l) = do
                t1 <- traexpr e1
                return $ C.TmProj t1 (g_name l)
        traexpr (T.RecordE fields) = do
                fields' <- forM fields $ \(li, ei) -> do
                        ti <- traexpr ei
                        return (g_name li, ti)
                return $ C.TmRecord fields'
        traexpr (T.CaseE e1 (Just ty2) alts) = do
                t1 <- traexpr e1
                tyT2 <- transType ctx ty2
                alts' <- forM alts $ \(pat, body) -> case pat of
                        T.ConP li ps -> do
                                xs <- (concat <$>) $
                                        forM ps $ \p -> case p of
                                                T.VarP x -> return [x]
                                                T.WildP -> return []
                                                T.ConP{} -> throwError "pattern argument" -- tmp
                                let ctx' = addNameList (map localName xs) ctx
                                ti <- transExpr knenv ctx' body
                                return (g_name li, (length xs, ti))
                        T.VarP x -> do
                                let ctx' = addName (localName x) ctx
                                ti <- transExpr knenv ctx' body
                                return (str2varName "", (1, ti))
                        T.WildP -> do
                                ti <- traexpr body
                                return (str2varName "", (0, ti))
                return $ C.TmCase (C.TmApp (C.TmUnfold tyT2) t1) alts'
        traexpr e = throwUnexpErr $ "illegal expression:" <+> pretty e

transType :: MonadThrow m => Context -> T.Type -> m C.Ty
transType ctx = tratype
    where
        tratype :: MonadThrow m => T.Type -> m C.Ty
        tratype (T.VarT tv) = do
                i <- getVarIndex ctx (localName $ tyVarLName tv)
                return $ C.TyVar i (length ctx)
        tratype (T.ConT x) = do
                i <- getVarIndex ctx x
                return $ C.TyVar i (length ctx)
        tratype (T.ArrT ty1 ty2) = do
                tyT1 <- tratype ty1
                tyT2 <- tratype ty2
                return $ C.TyArr tyT1 tyT2
        tratype (T.AllT xs ty) = do
                xs' <- forM xs $ \case
                        (tv, Just kn) -> do
                                knK <- transKind kn
                                return (tyVarName tv, knK)
                        _ -> throwUnexpErr "Kind inference failed"
                let ctx' = addNameList (map (localName . tyVarLName . fst) xs) ctx
                tyT2 <- transType ctx' ty
                return $ foldr (uncurry C.TyAll) tyT2 xs'
        tratype (T.AbsT x (Just kn) ty) = do
                knK1 <- transKind kn
                let ctx' = addName (localName x) ctx
                tyT2 <- transType ctx' ty
                return $ C.TyAbs (unLoc x) knK1 tyT2
        tratype (T.AppT ty1 ty2) = do
                tyT1 <- tratype ty1
                tyT2 <- tratype ty2
                return $ C.TyApp tyT1 tyT2
        tratype (T.RecT x kn ty) = do
                let ctx' = addName (localName x) ctx
                knK1 <- transKind kn
                tyT2 <- transType ctx' ty
                return $ C.TyRec (unLoc x) knK1 tyT2
        tratype (T.RecordT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (g_name l,) <$> tratype field
                return $ C.TyRecord fields'
        tratype (T.SumT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (unLoc l,) <$> mapM tratype field
                return $ C.TyVariant fields'
        tratype T.MetaT{} = throwUnexpErr "Zonking failed"
        tratype ty = throwUnexpErr $ "Illegal type:" <+> pretty ty

transKind :: MonadThrow m => T.Kind -> m C.Kind
transKind T.StarK = return C.KnStar
transKind T.MetaK{} = throwUnexpErr "Kind inference failed"
transKind (T.ArrK kn1 kn2) = C.KnArr <$> transKind kn1 <*> transKind kn2

transDecl :: (MonadThrow m, MonadIO m) => ModuleName -> Located T.Decl -> StateT (Context, T.KnEnv) m (Name, C.Binding)
transDecl modn (L sp dec) = do
        (ctx, knenv) <- get
        case dec of
                T.TypeD con ty -> do
                        ty' <- renameRecT ty
                        (ty'', kn) <- inferKind knenv ty'
                        let knenv' = M.insert (toplevelName modn con) kn knenv
                        knK <- transKind kn
                        tyT <- transType ctx ty''
                        let ctx' = addBinding (toplevelName modn con) (C.TyAbbBind tyT knK) ctx
                        put (ctx', knenv')
                        return (unLoc con, C.TyAbbBind tyT knK)
                T.VarD var ty -> do
                        ty' <- checkKindStar knenv sp ty
                        tyT <- transType ctx ty'
                        let ctx' = addName (toplevelName modn var) ctx
                        put (ctx', knenv)
                        return (unLoc var, C.VarBind tyT)
                T.ConD (T.FuncD var exp ty) -> do
                        ty' <- checkKindStar knenv sp ty
                        t <- transExpr knenv ctx exp
                        tyT <- transType ctx ty'
                        let ctx' = addName (toplevelName modn var) ctx
                        put (ctx', knenv)
                        return (unLoc var, C.TmAbbBind t tyT)

transTopFuncD :: (MonadThrow m, MonadIO m) => ModuleName -> Context -> T.KnEnv -> T.FuncD -> m ((Name, C.Binding), Context)
transTopFuncD modn ctx knenv (T.FuncD x e ty) = do
        ty' <- checkKindStar knenv NoSpan ty
        t <- transExpr knenv ctx (T.AbsE x (Just ty') e)
        tyT <- transType ctx ty'
        let ctx' = addName (toplevelName modn x) ctx -- tmp
        return ((unLoc x, C.TmAbbBind (C.TmFix t) tyT), ctx')

transEval :: (MonadThrow m, MonadIO m) => T.KnEnv -> Context -> (Located T.Expr, T.Type) -> m C.Term
transEval knenv ctx (L _ e, ty) = do
        e' <- bundleEval e
        t <- transExpr knenv ctx e'
        _ <- transType ctx ty -- tmp
        return t

typ2core :: (MonadThrow m, MonadIO m) => T.Program -> Plato m C.Module
typ2core (T.Program modn binds fundecs exps) = do
        fundec <- bundleTopFuncDs modn fundecs
        -- error $ show $ pretty fundec
        ctx <- gets plt_glbContext
        knenv <- gets plt_knEnv
        (binds', (ctx', knenv')) <- mapM (transDecl modn) binds `runStateT` (ctx, knenv)
        (funbind, ctx'') <- transTopFuncD modn ctx' knenv' fundec
        evals <- mapM (transEval knenv' ctx'') exps
        modify $ \s -> s{plt_knEnv = knenv'}
        return (C.Module modn (binds' ++ [funbind]) evals)