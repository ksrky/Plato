module Plato.Typing.Tc.Unify (
        UnificationError (..),
        InfiniteTypeError (..),
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
import Plato.Typing.Monad
import Plato.Typing.Tc.Utils

data UnificationError = UnificationError Tau Tau deriving (Show)
data InfiniteTypeError = InfiniteTypeError Tau Tau deriving (Show)

instance Exception UnificationError
instance Exception InfiniteTypeError

unify :: forall ctx m. (MonadReader ctx m, MonadIO m, MonadThrow m) => Tau -> Tau -> m ()
unify = unify'
    where
        unify' :: Tau -> Tau -> m ()
        unify' ty1 ty2 | badType ty1 || badType ty2 = throw (UnificationError ty1 ty2)
        unify' (VarT tv1) (VarT tv2) | tv1 == tv2 = return ()
        unify' (ConT tc1) (ConT tc2) | tc1 == tc2 = return ()
        unify' (ArrT arg1 res1) (ArrT arg2 res2) = do
                unify' (unLoc arg1) (unLoc arg2)
                unify' (unLoc res1) (unLoc res2)
        unify' (AppT fun1 arg1) (AppT fun2 arg2) = do
                unify' (unLoc fun1) (unLoc fun2)
                unify' (unLoc arg1) (unLoc arg2)
        unify' (MetaT tv1) (MetaT tv2) | tv1 == tv2 = return ()
        unify' (MetaT tv) ty = unifyVar tv ty
        unify' ty (MetaT tv) = unifyVar tv ty
        unify' ty1 ty2 = throw (UnificationError ty1 ty2)

        unifyVar :: MetaTv -> Tau -> m ()
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

        occursCheck :: MetaTv -> Tau -> m ()
        occursCheck tv1 ty2 = do
                tvs2 <- getMetaTvs ty2
                when (tv1 `S.member` tvs2) $ do
                        mb_ty1 <- readMetaTv tv1
                        let ty1 = case mb_ty1 of Just ty1 -> ty1; Nothing -> MetaT tv1
                        throw $ InfiniteTypeError ty1 ty2

        badType :: Tau -> Bool
        badType (VarT (BoundTv _)) = True
        badType _ = False

unifyFun :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Rho -> m (Sigma, Rho)
unifyFun (ArrT arg res) = return (unLoc arg, unLoc res)
unifyFun tau = do
        arg_ty <- newTyVar
        res_ty <- newTyVar
        unify tau (ArrT (noLoc arg_ty) (noLoc res_ty))
        return (arg_ty, res_ty)

unifyFuns :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Int -> Rho -> m ([Sigma], Rho)
unifyFuns 0 rho = return ([], rho)
unifyFuns n (ArrT arg res) = do
        (args, res') <- unifyFuns (n - 1) (unLoc res)
        return (unLoc arg : args, res')
unifyFuns n tau = do
        arg <- newTyVar
        res <- newTyVar
        unify tau (ArrT (noLoc arg) (noLoc res))
        (args, res') <- unifyFuns (n - 1) res
        return (arg : args, res')