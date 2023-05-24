module Plato.Typing.Tc.InstGen (
        instantiate,
        skolemise,
        generalize,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set qualified as S

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Monad
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.Subst
import Plato.Typing.Tc.Utils
import Plato.Typing.Zonking

-- | Instantiation
instantiate :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Sigma -> m (Coercion, Rho)
instantiate (AllT tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        return (instTrans tys, subst (map fst tvs) tys (unLoc tau))
instantiate ty = return (Id, ty)

-- | Soklemisation
skolemise ::
        (MonadReader ctx m, HasUniq ctx, MonadIO m) =>
        Sigma ->
        m (Coercion, [Quant], Rho)
skolemise (AllT tvs rho) = do
        sks1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newSkolemTyVar tv) tvs
        (coercion, sks2, ty') <- skolemise (subst (map fst tvs) (map (VarT . fst) sks1) (unLoc rho))
        return (prpolyTrans sks1 coercion, sks1 ++ sks2, ty')
skolemise (ArrT arg_ty res_ty) = do
        (coer, sks, res_ty') <- skolemise (unLoc res_ty)
        coer' <- prfunTrans sks (unLoc arg_ty) coer
        return (coer', sks, ArrT arg_ty (noLoc res_ty'))
skolemise ty = return (Id, [], ty)

-- | Generalization
generalize ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m) =>
        Rho ->
        m ([Quant], Sigma)
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        quantify (S.toList all_tvs) ty

quantify ::
        (MonadReader ctx m, HasUniq ctx, MonadIO m) =>
        [MetaTv] ->
        Rho ->
        m ([Quant], Sigma)
quantify [] ty = return ([], ty)
quantify tvs ty = do
        new_bndrs <- mapM ((BoundTv <$>) . freshIdent . str2tyvarName) $ take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map VarT new_bndrs)
        ty' <- zonkType ty
        qnts <- mapM (\tv -> (tv,) <$> newKnVar) new_bndrs
        return (qnts, AllT qnts (noLoc ty'))

allBinders :: [String]
allBinders =
        [[x] | x <- ['a' .. 'z']]
                ++ [x : show i | i <- [1 :: Integer ..], x <- ['a' .. 'z']]