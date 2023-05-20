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
import Plato.Core.Monad
import Plato.Driver.Monad
import Plato.Syntax.Core qualified as C
import Plato.Syntax.Typing qualified as T

elabExpr :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Expr -> m C.Term
elabExpr (T.VarE var) = do
        i <- getVarIndex (nameIdent var)
        return $ C.TmVar i (C.mkInfo var)
elabExpr (T.AppE fun arg) = C.TmApp <$> elabExpr (unLoc fun) <*> elabExpr (unLoc arg)
elabExpr (T.AbsE var (Just ty) body) = do
        tyT1 <- elabType ty
        t2 <- extendNameWith (nameIdent var) $ elabExpr (unLoc body)
        return $ C.TmAbs (C.mkInfo var) tyT1 t2
elabExpr (T.AbsE _ Nothing _) = unreachable ""
elabExpr (T.TAppE fun argtys) = do
        t1 <- elabExpr (unLoc fun)
        tys2 <- mapM elabType argtys
        return $ foldl C.TmTApp t1 tys2
elabExpr (T.TAbsE qnts body) = do
        qnts' <- forM qnts $ \(tv, kn) -> return (T.unTyVar tv, elabKind kn)
        t1 <- extendNameListWith (map (nameIdent . fst) qnts') $ elabExpr (unLoc body)
        return $ foldr (\(x, kn) -> C.TmTAbs (C.mkInfo x) kn) t1 qnts'
elabExpr (T.LetE bnds spcs body) = do
        bnds' <- mapM (\(id, exp) -> (nameIdent id,) <$> elabExpr (unLoc exp)) bnds
        spcs' <- mapM (\(id, ty) -> (nameIdent id,) <$> elabType ty) spcs
        let rbnds = recursiveBinds bnds' spcs'
        t2 <- elabExpr (unLoc body)
        return $ foldr (\(xi, ti, _) -> C.TmLet xi ti) t2 rbnds

elabType :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Type -> m C.Type
elabType (T.VarT tv) = do
        i <- getVarIndex (nameIdent $ T.unTyVar tv)
        return $ C.TyVar i (C.mkInfo $ T.unTyVar tv)
elabType (T.ConT tc) = do
        i <- getVarIndex (nameIdent tc)
        return $ C.TyVar i (C.mkInfo tc)
elabType (T.ArrT ty1 ty2) = C.TyFun <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AllT qnts ty) = do
        args <- forM qnts $ \(tv, kn) -> return (T.unTyVar tv, elabKind kn)
        tyT2 <- extendNameListWith (map (nameIdent . fst) args) $ elabType (unLoc ty)
        return $ foldr (\(x, knK1) -> C.TyAll (C.mkInfo x) knK1) tyT2 args
elabType (T.AppT ty1 ty2) = C.TyApp <$> elabType (unLoc ty1) <*> elabType (unLoc ty2)
elabType (T.AbsT tv kn body) = do
        tyT2 <- extendNameWith (nameIdent tv) $ elabType (unLoc body)
        return $ C.TyAbs (C.mkInfo tv) (elabKind kn) tyT2
elabType T.MetaT{} = unreachable "Zonking failed"

elabKind :: HasCallStack => T.Kind -> C.Kind
elabKind T.StarK = C.KnStar
elabKind (T.ArrK kn1 kn2) = C.KnFun (elabKind kn1) (elabKind kn2)
elabKind T.MetaK{} = unreachable "Kind inference failed"

elabBind :: (HasCallStack, MonadReader ctx m, HasCoreEnv ctx) => T.Bind -> m C.Command
elabBind (T.ValBind id exp) = undefined {-do
                                        t <- elabExpr (unLoc exp)
                                        tyT <- getType =<< getVarIndex (nameIdent id)
                                        return $ C.Bind (C.mkInfo id) (C.TmAbbBind t tyT)-}
elabBind (T.TypBind id ty) = do
        tyT <- elabType (unLoc ty)
        knK <- getKind =<< getVarIndex (nameIdent id)
        return $ C.Bind (C.mkInfo id) (C.TyAbbBind tyT knK)
elabBind (T.DatBind id params constrs) = do
        knK <- getKind =<< getVarIndex (nameIdent id)
        constrs' <- mapM (\(id, ty) -> (id,) <$> elabType (unLoc ty)) constrs
        return $ C.Bind (C.mkInfo id) (C.TyAbbBind undefined knK)

elabSpec :: (MonadReader ctx m, HasCoreEnv ctx) => T.Spec -> m C.Command
elabSpec (T.ValSpec id ty) = undefined
{-do
ty' <- elabType (unLoc ty)
return $ C.Bind (C.mkInfo id) (C.TmVarBind ty')-}
elabSpec (T.TypSpec id kn) = do
        let knK = elabKind kn
        return $ C.Bind (C.mkInfo id) (C.TyVarBind knK)

elabDecl :: (MonadReader ctx m, HasCoreEnv ctx) => T.Decl -> m C.Command
elabDecl (T.BindDecl bnd) = elabBind bnd
elabDecl (T.SpecDecl spc) = elabSpec spc

elabDecls :: (MonadReader ctx m, HasCoreEnv ctx) => [T.Decl] -> m [C.Command]
elabDecls decs = do
        let fspcs = [(id, unLoc ty) | T.SpecDecl (T.ValSpec id ty) <- decs]
            fbnds = [(id, unLoc exp) | T.BindDecl (T.ValBind id exp) <- decs]
        fspcs' <- mapM (\(id, ty) -> (nameIdent id,) <$> elabType ty) fspcs
        fbnds' <- mapM (\(id, exp) -> (nameIdent id,) <$> elabExpr exp) fbnds
        let rbnds = recursiveBinds fbnds' fspcs'
        cmds <- mapM elabDecl decs
        return $ map (\(xi, ti, tyTi) -> C.Bind xi (C.TmAbbBind ti tyTi)) rbnds ++ cmds

typ2core :: PlatoMonad m => T.Program -> m [C.Command]
typ2core (decs, exps) = do
        let cmds1 = runReader (mapM elabDecl decs) initCoreEnv
            cmds2 = runReader (mapM ((C.Eval <$>) . elabExpr . unLoc) exps) initCoreEnv
        return $ cmds1 ++ cmds2