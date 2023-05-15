{-# LANGUAGE LambdaCase #-}

module Plato.TypToCore where

import Control.Monad
import Control.Monad.Reader

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Core.Env
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabExpr :: T.Expr -> Reader CoreEnv C.Term
elabExpr (T.VarE var) = do
        i <- getVarIndex var
        return $ C.TmVar i (C.mkInfo var)
elabExpr (T.AppE fun arg) = C.TmApp <$> elabExpr (unLoc fun) <*> elabExpr (unLoc arg)
elabExpr (T.AbsE id (Just ty) body) = do
        tyT1 <- elabType ty
        t2 <- addNameWith id $ elabExpr (unLoc body)
        return $ C.TmAbs (C.mkInfo id) tyT1 t2
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
        t1 <- addNameListWith (map fst qnts') $ elabExpr (unLoc body)
        return $ foldr (\(x, kn) -> C.TmTAbs (C.mkInfo x) kn) t1 qnts'
elabExpr (T.LetE bnds sigs body) = do
        undefined

elabType :: T.Type -> Reader CoreEnv C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (T.unTyVar tv)
        return $ C.TyVar i (C.mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = do
        i <- getVarIndex tc
        return $ C.TyVar i (C.mkInfo tc)
elabType (T.ArrT ty1 ty2) = C.TyFun <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AllT tvs ty) = do
        args <- forM tvs $ \case
                (tv, Just kn) -> do
                        let knK = elabKind kn
                        return (T.unTyVar tv, knK)
                _ -> unreachable "Kind inference failed"
        tyT2 <- addNameListWith (map fst args) $ elabType (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (C.mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) =
        C.TyApp <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: T.Kind -> C.Kind
elabKind T.StarK = C.KnStar
elabKind (T.ArrK kn1 kn2) = C.KnFun (elabKind kn1) (elabKind kn2)
elabKind T.MetaK{} = unreachable "Kind inference failed"

elabBind :: T.Bind -> Reader CoreEnv C.Command
elabBind (T.ValBind id (Just ty) exp) = do
        exp' <- elabExpr (unLoc exp)
        ty' <- elabType ty
        return $ C.Bind (nameIdent id) (C.TmAbbBind exp' ty')
elabBind T.ValBind{} = unreachable ""

{- elabBind (T.TypBind id (Just kn) ty) = do
        ty' <- elabExpr (unLoc ty)
        let kn' = elabType kn
        return $ C.Bind id (C.TyAbbBind ty kn) -}

elabSpec :: T.Spec -> Reader CoreEnv C.Command
elabSpec (T.ValSpec id ty) = do
        ty' <- elabType (unLoc ty)
        return $ C.Bind (nameIdent id) (C.TmVarBind ty')
elabSpec (T.TypSpec id kn) = do
        let kn' = elabKind kn
        return $ C.Bind (nameIdent id) (C.TyVarBind kn')

elabDecl :: T.Decl -> Reader CoreEnv C.Command
elabDecl (T.BindDecl bnd) = elabBind bnd
elabDecl (T.SpecDecl spc) = elabSpec spc