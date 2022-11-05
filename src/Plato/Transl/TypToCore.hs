{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Transl.TypToCore where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T
import Plato.Typing.KindInfer
import Plato.Typing.Rename
import Plato.Typing.TcTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Plato.Common.GlbName

transExpr :: (MonadThrow m, MonadIO m) => KnTable -> Context -> T.Expr -> m C.Term
transExpr knenv ctx = traexpr
    where
        traexpr :: (MonadThrow m, MonadIO m) => T.Expr -> m C.Term
        traexpr (T.VarE x) = do
                i <- getVarIndex ctx x
                return $ C.TmVar i (length ctx)
        traexpr (T.AbsE x (Just ty1) e2) = do
                tyT1 <- transType ctx ty1
                ctx' <- addName x ctx
                t2 <- transExpr knenv ctx' e2
                return $ C.TmAbs x tyT1 t2
        traexpr (T.AppE e1 e2) = do
                t1 <- traexpr e1
                t2 <- traexpr e2
                return $ C.TmApp t1 t2
        traexpr (T.TAppE e1 tys) = do
                t1 <- traexpr e1
                tyTs' <- mapM (transType ctx) tys
                return $ foldl C.TmTApp t1 tyTs'
        traexpr (T.TAbsE xs e1) = do
                ctx' <- foldM (flip addName) ctx xs
                t1 <- transExpr knenv ctx' e1
                return $ foldr C.TmTAbs t1 xs
        traexpr (T.LetE [T.FuncD x e11 ty12] e2) = do
                ty12' <- checkKindStar knenv NoSpan ty12
                tyT1 <- transType ctx ty12'
                ctx' <- addName x ctx
                t1 <- transExpr knenv ctx' e11
                let t1' = C.TmFix (C.TmAbs x tyT1 t1)
                t2 <- transExpr knenv ctx' e2
                return $ C.TmLet x t1' t2
        traexpr (T.TagE l as ty) = do
                as' <- mapM traexpr as
                tyT <- transType ctx ty
                return $ C.TmTag l as' tyT
        traexpr (T.FoldE ty1) = C.TmFold <$> transType ctx ty1
        traexpr (T.ProjE e1 l) = do
                t1 <- traexpr e1
                return $ C.TmProj t1 l
        traexpr (T.RecordE fields) = do
                fields' <- forM fields $ \(li, ei) -> do
                        ti <- traexpr ei
                        return (li, ti)
                return $ C.TmRecord fields'
        traexpr (T.CaseE e1 (Just ty2) alts) = do
                t1 <- traexpr e1
                tyT2 <- transType ctx ty2
                alts' <- forM alts $ \(pat, body) -> case pat of
                        T.ConP li ps -> do
                                xs <- (concat <$>) $ forM ps $ \p -> case p of
                                        T.VarP x -> return [x]
                                        T.WildP -> return []
                                        T.ConP{} -> throwString "pattern argument" --tmp
                                ctx' <- foldM (flip addName) ctx xs
                                ti <- transExpr knenv ctx' body
                                return (li, (length xs, ti))
                        T.VarP x -> do
                                ctx' <- addName x ctx
                                ti <- transExpr knenv ctx' body
                                return (newName $ str2varName "", (1, ti))
                        T.WildP -> do
                                ti <- traexpr body
                                return (newName $ str2varName "", (0, ti))
                return $ C.TmCase (C.TmApp (C.TmUnfold tyT2) t1) alts'
        traexpr e = throwUnexpectedErr $ "illegal expression: " ++ show e

transType :: MonadThrow m => Context -> T.Type -> m C.Ty
transType ctx = tratype
    where
        tratype :: MonadThrow m => T.Type -> m C.Ty
        tratype (T.VarT tv) = do
                i <- getVarIndex ctx (tyVarName tv)
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
                        _ -> throwUnexpectedErr "Kind inference failed"
                ctx' <- addNames (map (tyVarName . fst) xs) ctx
                tyT2 <- transType ctx' ty
                return $ foldr (uncurry C.TyAll) tyT2 xs'
        tratype (T.AbsT x (Just kn) ty) = do
                knK1 <- transKind kn
                ctx' <- addName x ctx
                tyT2 <- transType ctx' ty
                return $ C.TyAbs x knK1 tyT2
        tratype (T.AppT ty1 ty2) = do
                tyT1 <- tratype ty1
                tyT2 <- tratype ty2
                return $ C.TyApp tyT1 tyT2
        tratype (T.RecT x kn ty) = do
                ctx' <- addName x ctx
                knK1 <- transKind kn
                tyT2 <- transType ctx' ty
                return $ C.TyRec x knK1 tyT2
        tratype (T.RecordT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (l,) <$> tratype field
                return $ C.TyRecord fields'
        tratype (T.SumT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (l,) <$> mapM tratype field
                return $ C.TyVariant fields'
        tratype T.MetaT{} = throwUnexpectedErr "Zonking failed"
        tratype ty = throwUnexpectedErr $ "Illegal type: " ++ show ty

transKind :: MonadThrow m => T.Kind -> m C.Kind
transKind T.StarK = return C.KnStar
transKind T.MetaK{} = throwString "Kind inference failed" --tmp
transKind (T.ArrK kn1 kn2) = C.KnArr <$> transKind kn1 <*> transKind kn2

transKind' :: C.Kind -> T.Kind
transKind' C.KnStar = T.StarK
transKind' (C.KnArr knK1 knK2) = T.ArrK (transKind' knK1) (transKind' knK2)

transDecl :: (MonadThrow m, MonadIO m) => Located T.Decl -> (Context, KnTable) -> m ((GlbName, C.Binding), (Context, KnTable))
transDecl (L sp dec) (ctx, knenv) = case dec of
        T.TypeD name ty -> do
                (ty', kn) <- inferKind knenv ty
                let knenv' = M.insert name kn knenv
                knK <- transKind kn
                tyT <- transType ctx ty'
                ctx' <- addBinding name (C.TyAbbBind (L sp tyT) knK) ctx
                return ((name, C.TyAbbBind (L sp tyT) knK), (ctx', knenv'))
        T.VarD x ty -> do
                ty' <- checkKindStar knenv sp ty
                tyT <- transType ctx ty'
                ctx' <- addName x ctx
                return ((x, C.VarBind $ L sp tyT), (ctx', knenv))
        T.ConD (T.FuncD f e ty) -> do
                ty' <- checkKindStar knenv sp ty
                t <- transExpr knenv ctx e
                tyT <- transType ctx ty'
                ctx' <- addName f ctx
                return ((f, C.TmAbbBind (noLoc t) (L sp tyT)), (ctx', knenv))

transTopFuncDs :: (MonadThrow m, MonadIO m) => T.FuncD -> (Context, KnTable) -> m ((GlbName, C.Binding), Context)
transTopFuncDs (T.FuncD x e ty) (ctx, knenv) = do
        ty' <- checkKindStar knenv NoSpan ty
        t <- transExpr knenv ctx (T.AbsE x (Just ty') e)
        tyT <- transType ctx ty'
        ctx' <- addName x ctx
        return ((x, C.TmAbbBind (noLoc $ C.TmFix t) (L NoSpan tyT)), ctx')

transEval :: (MonadThrow m, MonadIO m) => RenameState -> KnTable -> Context -> (Located T.Expr, T.Type) -> m (Located C.Term)
transEval st knenv ctx (L sp e, ty) = do
        e' <- rename st e
        t <- transExpr knenv ctx e'
        _ <- transType ctx ty --tmp
        return (L sp t)

typ2core :: (MonadThrow m, MonadIO m) => Names -> Context -> T.Program -> m (Names, [C.Command])
typ2core ns ctx (T.Program modn binds fundecs exps) = do
        (fundec, st) <- renameFuncDs emptyRenameState{moduleName = modn, externalNames = ns} fundecs
        let knenv = M.fromList [(x, transKind' knK) | (x, C.TyAbbBind _ knK) <- V.toList ctx]
        (binds', (ctx', _)) <- mapM (StateT . transDecl) binds `runStateT` (ctx, knenv)
        let knenv' = knenv `M.union` M.fromList [(x, transKind' knK) | (x, C.TyAbbBind _ knK) <- V.toList (V.take (length binds) ctx')]
        (funbind, ctx'') <- transTopFuncDs fundec (ctx', knenv')
        body <- mapM (transEval st knenv' ctx'') exps
        return (names st, map (uncurry C.Bind) (binds' ++ [funbind]) ++ map C.Eval body)
