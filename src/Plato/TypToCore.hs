{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore where

{-
import Control.Exception.Safe
import Control.Monad
import Control.Monad.RWS
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Debug
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabPath :: Path -> Reader Context C.Term
elabPath (PIdent id) = do
        i <- getVarIndex id
        return $ C.TmVar i (mkInfo id)
elabPath (PDot root field) = do
        root' <- elabPath root
        return $ C.TmProj root' (unLoc field)

elabExpr :: T.Expr -> Reader Context C.Term
elabExpr (T.VarE p) = elabPath p
    where
        elabPath :: Path -> Reader Context C.Term
        elabPath (PIdent id) = do
                i <- getVarIndex id
                return $ C.TmVar i (mkInfo id)
        elabPath (PDot root field) = do
                root' <- elabPath root
                return $ C.TmProj root' (unLoc field)
elabExpr (T.AppE fun arg) = do
        t1 <- elabExpr (unLoc fun)
        t2 <- elabExpr (unLoc arg)
        return $ C.TmApp t1 t2
elabExpr (T.AbsE id (Just ty) body) = do
        tyT1 <- elabType ty
        t2 <- local (addName id) $ elabExpr (unLoc body)
        return $ C.TmAbs (mkInfo id) tyT1 t2
elabExpr (T.AbsE _ Nothing _) = unreachable ""
elabExpr (T.PAbsE pat (Just ty) body) = undefined
elabExpr (T.PAbsE _ Nothing _) = unreachable ""
elabExpr (T.TAppE fun argtys) = do
        t1 <- elabExpr (unLoc fun)
        tys2 <- mapM elabType argtys
        return $ foldl C.TmTApp t1 tys2
elabExpr (T.TAbsE qnts body) = do
        qnts' <- forM qnts $ \case
                (tv, Just kn) -> do
                        let knK = elabKind kn
                        return (T.unTyVar tv, knK)
                (_, Nothing) -> unreachable "Kind inference failed"
        t1 <- local (\ctx -> foldr addName ctx (map fst qnts')) $ elabExpr (unLoc body)
        return $ foldr (\(x, kn) -> C.TmTAbs (mkInfo x) kn) t1 qnts'
elabExpr (T.LetE decs e2) = undefined
elabExpr T.MatchE{} = undefined

elabType :: T.Type -> Reader Context C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (T.unTyVar tv)
        return $ C.TyVar i (mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = elabPath tc
    where
        elabPath :: Path -> Reader Context C.Type
        elabPath (PIdent id) = do
                i <- getVarIndex id
                return $ C.TyVar i (mkInfo id)
        elabPath (PDot root field) = do
                root' <- elabPath root
                return $ C.TmProj root' (unLoc field)
elabType (T.ArrT ty1 ty2) = do
        tyT1 <- elabType (unLoc ty1)
        tyT2 <- elabType (unLoc ty2)
        return $ C.TyArr tyT1 tyT2
elabType (T.AllT tvs ty) = do
        args <- forM tvs $ \case
                (tv, Just kn) -> do
                        knK <- transKind kn
                        return (T.unTyVar tv, knK)
                _ -> unreachable "Kind inference failed"
        let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
        tyT2 <- transType ctx' (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) = do
        tyT1 <- elabType (unLoc ty1)
        tyT2 <- elabType (unLoc ty2)
        return $ C.TyApp tyT1 tyT2
elabType (T.AbsT x (Just kn) ty) = do
        knK1 <- transKind kn
        let ctx' = addName (unLoc x) ctx
        tyT2 <- transType ctx' (unLoc ty)
        return $ C.TyAbs (mkInfo x) knK1 tyT2
elabType (T.AbsT x Nothing ty) = undefined
{-elabType (T.RecT x (Just kn) ty) = do
        knK1 <- transKind kn
        let ctx' = addName (unLoc x) ctx
        tyT2 <- transType ctx' (unLoc ty)
        return $ C.TyRec (mkInfo x) knK1 tyT2-}
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: T.Kind -> C.Kind
elabKind = undefined-}