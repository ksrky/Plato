{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Tc.Unify (
        unify,
        unifyFun,
        unifyFuns,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Error
import Plato.Typing.Misc

unify :: (MonadReader e m, MonadIO m, MonadThrow m) => Tau -> Tau -> m ()
unify ty1 ty2 | badType ty1 || badType ty2 = throw UnificationFail
unify (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
unify (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
unify (ArrT arg1 res1) (ArrT arg2 res2) = do
        unify (unLoc arg1) (unLoc arg2)
        unify (unLoc res1) (unLoc res2)
unify (AppT fun1 arg1) (AppT fun2 arg2) = do
        unify (unLoc fun1) (unLoc fun2)
        unify (unLoc arg1) (unLoc arg2)
unify (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
unify (MetaT tv) ty = unifyVar tv ty
unify ty (MetaT tv) = unifyVar tv ty
unify _ _ = throw UnificationFail

unifyVar :: (MonadReader e m, MonadIO m, MonadThrow m) => MetaTv -> Tau -> m ()
unifyVar tv1 ty2 = do
        mb_ty1 <- readMetaTv tv1
        case (mb_ty1, ty2) of
                (Just ty1, _) -> unify ty1 ty2
                (Nothing, MetaT tv2) ->
                        readMetaTv tv2 >>= \case
                                Just kn2 -> unify (MetaT tv1) kn2
                                Nothing -> writeMetaTv tv1 ty2
                (Nothing, _) -> do
                        occursCheck tv1 ty2
                        writeMetaTv tv1 ty2

occursCheck :: (MonadReader e m, MonadIO m, MonadThrow m) => MetaTv -> Tau -> m ()
occursCheck tv1 ty2 = do
        tvs2 <- getMetaTvs ty2
        when (tv1 `S.member` tvs2) $ throw OccursCheckFail

badType :: Tau -> Bool
badType (VarT (BoundTv _)) = True
badType _ = False

unifyFun :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Rho -> m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

unifyFuns :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Int -> Rho -> m ([Sigma], Rho)
unifyFuns 0 rho = return ([], rho)
unifyFuns n (ArrT arg res) = do
        (args, res') <- unifyFuns (n - 1) (unLoc res)
        return (unLoc arg : args, res')
unifyFuns n tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        (args, res') <- unifyFuns (n - 1) res_ty
        return (arg_ty : args, res')