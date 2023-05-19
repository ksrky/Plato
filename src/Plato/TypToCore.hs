{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.TypToCore (typ2core) where

import Control.Monad
import Control.Monad.Reader
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Core.Elab
import Plato.Core.Env
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabExpr :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Expr -> m C.Term
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
elabExpr (T.LetE bnds spcs body) = do
        bnds' <- mapM (\(id, exp) -> (nameIdent id,) <$> elabExpr (unLoc exp)) bnds
        spcs' <- mapM (\(id, ty) -> (nameIdent id,) <$> elabType ty) spcs
        let t1 = recursiveBinds bnds' spcs'
        t2 <- elabExpr (unLoc body)
        return $ C.TmLet C.Dummy t1 t2

elabType :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Type -> m C.Type
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
elabType (T.AppT ty1 ty2) = C.TyApp <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AbsT var kn body) = C.TyAbs (C.mkInfo var) (elabKind kn) <$> elabType (unLoc body)
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: HasCallStack => T.Kind -> C.Kind
elabKind T.StarK = C.KnStar
elabKind (T.ArrK kn1 kn2) = C.KnFun (elabKind kn1) (elabKind kn2)
elabKind T.MetaK{} = unreachable "Kind inference failed"

elabBind :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Bind -> m C.Command
elabBind (T.ValBind id (Just ty) exp) = do
        exp' <- elabExpr (unLoc exp)
        ty' <- elabType ty
        return $ C.Bind (C.mkInfo id) (C.TmAbbBind exp' ty')
elabBind T.ValBind{} = unreachable ""
elabBind (T.TypBind id (Just kn) ty) = do
        ty' <- elabType (unLoc ty)
        let kn' = elabKind kn
        return $ C.Bind (C.mkInfo id) (C.TyAbbBind ty' kn')
elabBind T.TypBind{} = unreachable ""

elabSpec :: (MonadReader ctx m, HasCoreEnv ctx) => T.Spec -> m C.Command
elabSpec (T.ValSpec id ty) = do
        ty' <- elabType (unLoc ty)
        return $ C.Bind (C.mkInfo id) (C.TmVarBind ty')
elabSpec (T.TypSpec id kn) = do
        let kn' = elabKind kn
        return $ C.Bind (C.mkInfo id) (C.TyVarBind kn')

elabDecl :: (MonadReader ctx m, HasCoreEnv ctx) => T.Decl -> m C.Command
elabDecl (T.BindDecl bnd) = elabBind bnd
elabDecl (T.SpecDecl spc) = elabSpec spc

typ2core :: PlatoMonad m => T.Program -> m [C.Command]
typ2core (decs, exps) = do
        let cmds1 = runReader (mapM elabDecl decs) initCoreEnv
            cmds2 = runReader (mapM ((C.Eval <$>) . elabExpr . unLoc) exps) initCoreEnv
        return $ cmds1 ++ cmds2