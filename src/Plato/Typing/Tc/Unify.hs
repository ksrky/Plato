{-# LANGUAGE OverloadedStrings #-}

module Plato.Typing.Tc.Unify (
        unify,
        unifyFun,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing.Type
import Plato.Typing.Monad
import Plato.Typing.Tc.Utils

unify :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Span -> Tau -> Tau -> m ()
unify sp = unify'
    where
        unify' :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Tau -> Tau -> m ()
        unify' ty1 ty2 | badType ty1 || badType ty2 = do
                throwLocErr sp $
                        vsep
                                [ "Couldn't match type."
                                , "Expected type:" <+> pretty ty2
                                , indent 2 ("Actual type:" <+> pretty ty1)
                                ]
        unify' (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
        unify' (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
        unify' (ArrT arg1 res1) (ArrT arg2 res2) = do
                unify' (unLoc arg1) (unLoc arg2)
                unify' (unLoc res1) (unLoc res2)
        unify' (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
        unify' (MetaT tv) ty = unifyVar tv ty
        unify' ty (MetaT tv) = unifyVar tv ty
        unify' ty1 ty2 =
                throwLocErr sp $
                        vsep
                                [ "Couldn't match type."
                                , "Expected type:" <+> pretty ty2
                                , indent 2 ("Actual type:" <+> pretty ty1)
                                ]

        unifyVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => MetaTv -> Tau -> m ()
        unifyVar tv1 ty2@(MetaT tv2) = do
                mb_ty1 <- readMetaTv tv1
                mb_ty2 <- readMetaTv tv2
                case (mb_ty1, mb_ty2) of
                        (Just ty1, _) -> unify' ty1 ty2
                        (Nothing, Just ty2) -> unify' (MetaT tv1) ty2
                        (Nothing, Nothing) -> writeMetaTv tv1 ty2
        unifyVar tv1 ty2 = do
                occursCheck tv1 ty2
                writeMetaTv tv1 ty2

        occursCheck :: (MonadReader ctx m, MonadThrow m, MonadIO m) => MetaTv -> Tau -> m ()
        occursCheck tv1 ty2 = do
                tvs2 <- getMetaTvs ty2
                when (tv1 `S.member` tvs2) $
                        throwLocErr sp $
                                hsep ["Infinite type:", squotes $ pretty ty2]

        badType :: Tau -> Bool
        badType (VarT (BoundTv _)) = True
        badType _ = False

unifyFun :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Span -> Rho -> m (Sigma, Rho)
unifyFun _ (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun err_sp tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify err_sp tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)