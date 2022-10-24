{-# LANGUAGE TupleSections #-}

module Plato.Transl.TypToCore where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Commands as C
import Plato.Core.Context
import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T
import Plato.Typing.KindInfer
import Plato.Typing.Renamer
import Plato.Typing.TrTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

transExpr :: MonadThrow m => Subst -> Context -> T.Expr -> m C.Term
transExpr subst ctx = traexpr
    where
        traexpr :: MonadThrow m => T.Expr -> m C.Term
        traexpr (T.VarE x) = do
                i <- getVarIndex ctx x
                return $ C.TmVar i (length ctx)
        traexpr (T.AbsE x (Just ty1) e2) = do
                tyT1 <- transType subst ctx ty1
                ctx' <- addName x ctx
                t2 <- transExpr subst ctx' (unLoc e2)
                return $ C.TmAbs (unLoc x) tyT1 t2
        traexpr (T.AppE e1 e2) = do
                t1 <- traexpr $ unLoc e1
                t2 <- traexpr $ unLoc e2
                return $ C.TmApp t1 t2
        traexpr (T.TAppE e1 tys) = do
                t1 <- traexpr $ unLoc e1
                tyTs' <- mapM (transType subst ctx) tys
                return $ foldl C.TmTApp t1 tyTs'
        traexpr exp@(T.TAbsE xs e1) = do
                xs' <- forM xs $ \(L sp x) -> do
                        knK <- case M.lookup x subst of
                                Just kn -> return $ transKind kn
                                _ -> throwLocatedErr sp $ "Kind inference failed for " ++ show x
                        return (x, knK)
                ctx' <- foldM (flip addName) ctx xs
                t2 <- transExpr subst ctx' $ unLoc e1
                return $ foldr (uncurry C.TmTAbs) t2 xs'
        traexpr (T.LetE [T.FD x e11 ty12] e2) = do
                tyT1 <- transType subst ctx $ unLoc ty12
                t1 <- traexpr $ unLoc e11
                let t1' = C.TmFix (C.TmAbs (unLoc x) tyT1 t1)
                ctx' <- addName x ctx
                t2 <- transExpr subst ctx' $ unLoc e2
                return $ C.TmLet (unLoc x) t1' t2
        traexpr (T.TagE l as (Just ty)) = do
                as' <- mapM (traexpr . unLoc) as
                tyT <- transType subst ctx ty
                return $ C.TmTag (unLoc l) as' tyT
        traexpr (T.ProjE e1 l) = do
                t1 <- traexpr $ unLoc e1
                return $ C.TmProj t1 (unLoc l)
        traexpr (T.RecordE fields) = do
                fields' <- forM fields $ \(li, ei) -> do
                        ti <- traexpr $ unLoc ei
                        return (unLoc li, ti)
                return $ C.TmRecord fields'
        traexpr (T.CaseE e1 (Just ty2) alts) = do
                t1 <- traexpr $ unLoc e1
                tyT2 <- transType nullSubst ctx ty2
                alts' <- forM alts $ \(pat, body) -> case unLoc pat of
                        T.ConP li ps -> do
                                as <- (concat <$>) <$> forM ps $ \(L sp p) -> case p of
                                        T.VarP x -> return [x]
                                        T.WildP -> return []
                                        T.ConP{} -> throwLocatedErr sp "pattern argument" --tmp
                                ctx' <- foldM (flip addName) ctx as
                                ti <- transExpr subst ctx' $ unLoc body
                                return (unLoc li, (length as, ti))
                        T.VarP x -> do
                                ctx' <- addName x ctx
                                ti <- traexpr $ unLoc body
                                return (str2varName "", (1, ti))
                        T.WildP -> do
                                ti <- traexpr $ unLoc body
                                return (str2varName "", (0, ti))
                return $ C.TmCase t1 alts'
        traexpr _ = undefined

transType :: MonadThrow m => Subst -> Context -> T.Type -> m C.Ty
transType subst ctx = tratype
    where
        tratype :: MonadThrow m => T.Type -> m C.Ty
        tratype (T.VarT tv) = do
                i <- getVarIndex ctx (tyVarName <$> tv)
                return $ C.TyVar i (length ctx)
        tratype (T.ConT x) = do
                i <- getVarIndex ctx x
                return $ C.TyVar i (length ctx)
        tratype (T.ArrT ty1 ty2) = do
                tyT1 <- tratype (unLoc ty1)
                tyT2 <- tratype (unLoc ty2)
                return $ C.TyArr tyT1 tyT2
        tratype (T.AllT xs ty) = do
                xs' <- forM xs $ \(L sp tv) -> do
                        knK <- case M.lookup (tyVarName tv) subst of
                                Just kn -> return $ transKind kn
                                _ -> throwLocatedErr sp $ "Kind inference failed for " ++ show (tyVarName tv)
                        return (tyVarName tv, knK)
                ctx' <- addNames (map (tyVarName <$>) xs) ctx
                tyT2 <- transType subst ctx' (unLoc ty)
                return $ foldr (uncurry C.TyAll) tyT2 xs'
        tratype (T.AbsT x ty) = do
                knK1 <- case M.lookup (unLoc x) subst of
                        Just knK -> return $ transKind knK
                        _ -> throwLocatedErr (getSpan x) $ "Kind inference failed for " ++ show x
                ctx' <- addName x ctx
                tyT2 <- transType subst ctx' (unLoc ty)
                return $ C.TyAbs (unLoc x) knK1 tyT2
        tratype (T.AppT ty1 ty2) = do
                tyT1 <- tratype $ unLoc ty1
                tyT2 <- tratype $ unLoc ty2
                return $ C.TyApp tyT1 tyT2
        tratype (T.RecT x ty) = do
                knK1 <- case M.lookup (unLoc x) subst of
                        Just knK -> return $ transKind knK
                        _ -> throwLocatedErr (getSpan x) $ "Kind inference failed for " ++ show x
                ctx' <- addName x ctx
                tyT2 <- transType subst ctx' (unLoc ty)
                return $ C.TyRec (unLoc x) knK1 tyT2 -- tmp: unused
        tratype (T.RecordT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (unLoc l,) <$> tratype (unLoc field)
                return $ C.TyRecord fields'
        tratype (T.SumT fieldtys) = do
                fields' <- forM fieldtys $ \(l, field) -> (unLoc l,) <$> mapM (tratype . unLoc) field
                return $ C.TyVariant fields'
        tratype T.MetaT{} = throwUnexpectedErr "Zonking failed"

transKind :: T.Kind -> C.Kind
transKind T.StarK = C.KnStar
transKind T.VarK{} = C.KnStar
transKind (T.ArrK k1 k2) = C.KnArr (transKind k1) (transKind k2)

transDecl :: MonadThrow m => T.Decl -> StateT Context m (Located Name, C.Binding)
transDecl dec = StateT $ \ctx -> case dec of
        T.TypeD name ty -> do
                (subst, kn) <- inferKind (unLoc ty)
                let knK = transKind kn
                tyT <- transType subst ctx `traverse` ty
                ctx' <- addName name ctx
                return ((name, C.TyAbbBind tyT (Just knK)), ctx')
        T.VarD f ty -> do
                subst <- checkKindStar ty
                tyT <- transType subst ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.VarBind tyT), ctx')
        T.FuncD (T.FD f e ty) -> do
                subst <- checkKindStar ty
                t <- transExpr subst ctx `traverse` e
                tyT <- transType subst ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.TmAbbBind t (Just tyT)), ctx')

typ2core :: MonadThrow m => Context -> T.Decls -> m Commands
typ2core ctx (T.Decls modn imps binds fundecs) = do
        (fundec, st) <- renameFuncDecls (initRenameState modn) fundecs
        (binds', ctx') <- mapM transDecl (map unLoc binds ++ [T.FuncD fundec]) `runStateT` ctx
        return $ Commands{C.imports = imps, C.binds = binds'}
