{-# LANGUAGE OverloadedStrings #-}

module Plato.TypeCheck.Unify where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S
import Prettyprinter

import Control.Exception.Safe
import Plato.Syntax.Typing
import Plato.TypeCheck.Monad
import Plato.TypeCheck.Utils
import Plato.Types.Location

unify :: (MonadThrow m, MonadIO m) => Tau -> Tau -> Tc m ()
unify ty1 ty2 | badType ty1 || badType ty2 = do
        sp <- readErrLoc
        throwTc sp $ vsep ["Couldn't match type.", "Expected type:" <+> pretty ty2, indent 2 ("Actual type:" <+> pretty ty1)]
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
unify ty1 ty2 = do
        sp <- readErrLoc
        throwTc sp $ vsep ["Couldn't match type.", "Expected type:" <+> pretty ty2, indent 2 ("Actual type:" <+> pretty ty1)]

unifyVar :: (MonadThrow m, MonadIO m) => MetaTv -> Tau -> Tc m ()
unifyVar tv1 ty2@(MetaT tv2) = do
        mb_ty1 <- readMetaTv tv1
        mb_ty2 <- readMetaTv tv2
        case (mb_ty1, mb_ty2) of
                (Just ty1, _) -> unify ty1 ty2
                (Nothing, Just ty2) -> unify (MetaT tv1) ty2
                (Nothing, Nothing) -> writeMetaTv tv1 ty2
unifyVar tv1 ty2 = do
        occursCheck tv1 ty2
        writeMetaTv tv1 ty2

unifyFun :: (MonadIO m, MonadThrow m) => Rho -> Tc m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

occursCheck :: (MonadThrow m, MonadIO m) => MetaTv -> Tau -> Tc m ()
occursCheck tv1 ty2 = do
        tvs2 <- getMetaTvs ty2
        when (tv1 `S.member` tvs2) $ do
                sp <- readErrLoc
                throwTc sp $ hsep ["Infinite type:", squotes $ pretty ty2]

badType :: Tau -> Bool
badType (VarT (BoundTv _)) = True
badType _ = False