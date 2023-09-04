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
import Plato.Typing.Misc
import Plato.Typing.Tc.Coercion

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
        qns1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newFreeTv tv) tvs
        (coercion, qns2, rho') <- skolemise (substTvs (map fst tvs) (map (VarT . fst) qns1) (unLoc rho))
        return (prpolyTrans qns1 coercion, qns1 ++ qns2, rho')
skolemise (ArrT arg_ty res_ty) = do
        (coer, sks, res_ty') <- skolemise (unLoc res_ty)
        coer' <- prfunTrans sks (unLoc arg_ty) coer
        return (coer', sks, ArrT arg_ty (noLoc res_ty'))
skolemise rho = return (mempty, [], rho)

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
quantify [] rho = return ([], rho)
quantify tvs rho = do
        new_bndrs <- mapM (const $ BoundTv <$> newVarIdent) tvs
        zipWithM_ writeMetaTv tvs (map VarT new_bndrs)
        qns <- mapM (\tv -> (tv,) <$> newKnVar) new_bndrs
        return (qns, AllT qns (noLoc rho))