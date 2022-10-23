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

transExpr = undefined

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
                checkKindStar ty
                tyT <- transType nullSubst ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.VarBind tyT), ctx')
        T.FuncD (T.FD f e ty) -> do
                checkKindStar ty
                t <- transExpr ctx ty e
                tyT <- transType nullSubst ctx `traverse` ty
                ctx' <- addName f ctx
                return ((f, C.TmAbbBind t (Just tyT)), ctx')

typ2core :: MonadThrow m => Context -> T.Decls -> m Commands
typ2core ctx (T.Decls modn imps binds fundecs) = do
        (fundec, st) <- renameFuncDecls (initRenameState modn) fundecs
        (binds', ctx') <- mapM transDecl (map unLoc binds ++ [T.FuncD fundec]) `runStateT` ctx
        return $ Commands{C.imports = imps, C.binds = binds'}
