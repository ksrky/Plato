{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore where


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

elabExpr :: T.Expr -> Reader Context C.Term
elabExpr (T.VarE var) = do
        i <- getVarIndex var
        return $ C.TmVar i (mkInfo var)
elabExpr (T.AppE fun arg) = return $ C.TmApp <$> elabExpr (unLoc fun) <*> elabExpr (unLoc arg)
elabExpr (T.AbsE id (Just ty) body) = do
        tyT1 <- elabType ty
        t2 <- addNameWith id $ elabExpr (unLoc body)
        return $ C.TmAbs (mkInfo id) tyT1 t2
elabExpr (T.AbsE _ Nothing _) = unreachable ""
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

elabType :: T.Type -> Reader Context C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (T.unTyVar tv)
        return $ C.TyVar i (mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = do
        i <- getVarIndex tc
        return $ C.TyVar i (mkInfo tc)
elabType (T.ArrT ty1 ty2) = C.TyArr <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AllT tvs ty) = do
        args <- forM tvs $ \case
                (tv, Just kn) -> do
                        knK <- transKind kn
                        return (T.unTyVar tv, knK)
                _ -> unreachable "Kind inference failed"
        let ctx' = foldl (flip addName) ctx (map (unLoc . fst) args)
        tyT2 <- transType ctx' (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) = return $ C.TyApp <$>  elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: T.Kind -> C.Kind
elabKind StarK = KnStar
elabKind (ArrK kn1 kn2) = KnFun (elabKind kn1) (elabKind kn2)
elabKind MetaT{} = unreachable "Kind inference failed"

elabBind :: T.Bind -> Reader CoreEnv Command
elabBind (T.ValBind x (Just ty) exp) = do
        exp' <- elabExpr (unLoc exp)
        ty' <- elabType ty
        return $ C.Bind x (C.TmAbbBind exp' ty')
{-elabBind (T.TypeBind x (Just kn) ty) = do
        ty' <- elabExpr (unLoc ty)
        let kn' = elabType kn
        return $ C.Bind x (C.TyAbbBind ty kn)-}
elabBind _ = unreachable ""

elabSpec :: T.Bind -> Reader CoreEnv Command
elabSpec (T.ValSpec x ty) = skip {- do
        ty' <- elabType ty
        return$ C.Bind x (C.VarBind ty') -}
elabSpec (T.TypeSpec x kn) = do
        let kn' = elabKind kn
        return $ C.Bind x (C.TyVarBind kn')

elabDecl :: T.Decl -> Reader CoreEnv Command
elabDecl (BindDecl bnd) = elabBind bnd
elabDecl (SpecDecl spc) = elabSpec spc
