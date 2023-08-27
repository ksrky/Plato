module Plato.Typing.Tc.InstGen (
        instantiate,
        skolemise,
        generalize,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.Utils
import Plato.Typing.Zonking

-- | Instantiation
instantiate :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> m (Coercion, Rho)
instantiate (AllT tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        return (instTrans tys, substTvs (map fst tvs) tys (unLoc tau))
instantiate ty = return (mempty, ty)

-- | Skolemisation
skolemise ::
        (MonadReader e m, HasUniq e, MonadIO m) =>
        Sigma ->
        m (Coercion, [Quant], Rho)
skolemise (AllT tvs rho) = do
        sks1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newSkolemTyVar tv) tvs
        (coercion, sks2, ty') <- skolemise (substTvs (map fst tvs) (map (VarT . fst) sks1) (unLoc rho))
        return (prpolyTrans sks1 coercion, sks1 ++ sks2, ty')
skolemise (ArrT arg_ty res_ty) = do
        (coer, sks, res_ty') <- skolemise (unLoc res_ty)
        coer' <- prfunTrans sks (unLoc arg_ty) coer
        return (coer', sks, ArrT arg_ty (noLoc res_ty'))
skolemise ty = return (mempty, [], ty)

-- | Generalization
generalize ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m) =>
        Rho ->
        m ([Quant], Sigma)
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        quantify (S.toList all_tvs) ty

quantify ::
        (MonadReader e m, HasUniq e, MonadIO m) =>
        [MetaTv] ->
        Rho ->
        m ([Quant], Sigma)
quantify [] ty = return ([], ty)
quantify tvs ty = do
        new_bndrs <- mapM (const $ BoundTv <$> newVarIdent) tvs
        zipWithM_ writeMetaTv tvs (map VarT new_bndrs)
        ty' <- zonk ty
        qnts <- mapM (\tv -> (tv,) <$> newKnVar) new_bndrs
        return (qnts, AllT qnts (noLoc ty'))