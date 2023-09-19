module Plato.Typing.Tc.InstGen (
        instantiate,
        skolemise,
        generalize,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.List qualified as List
import Data.Set qualified as S

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Misc
import Plato.Typing.Tc.Coercion

-- | Instantiation
instantiate :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> m (Coercion, Rho)
instantiate (AllT qns tau) = do
        tys <- mapM (const newTyVar) qns
        tau' <- substTvs (map fst qns) tys (unLoc tau)
        return (instTrans tys, tau')
instantiate ty = return (mempty, ty)

-- | Skolemisation
skolemise ::
        (MonadReader e m, HasUniq e, MonadIO m) =>
        Sigma ->
        m (Coercion, [Quant], Rho)
skolemise (AllT qns rho) = do
        qns1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newFreeTv tv) qns
        (coer, qns2, rho') <- skolemise =<< substTvs (map fst qns) (map (VarT . fst) qns1) (unLoc rho)
        return (prpolyTrans qns1 coer, qns1 ++ qns2, rho')
skolemise (ArrT arg_ty res_ty) = do
        (coer, qns, res_ty') <- skolemise (unLoc res_ty)
        coer' <- prfunTrans qns (unLoc arg_ty) coer
        return (coer', qns, ArrT arg_ty (noLoc res_ty'))
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
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m) =>
        [MetaTv] ->
        Rho ->
        m ([Quant], Sigma)
quantify [] rho = return ([], rho)
quantify tvs rho = do
        ftvs <- mconcat <$> (mapM getFreeTvs =<< getEnvTypes)
        let ftvnames = map (nameIdent . unTyVar) (S.toList ftvs)
        newtvs <- zipWithM (\n -> const $ BoundTv <$> freshIdent n) (nameSupply List.\\ ftvnames) tvs
        zipWithM_ writeMetaTv tvs (map VarT newtvs)
        qns <- mapM (\tv -> (tv,) <$> newKnVar) newtvs
        return (qns, AllT qns (noLoc rho))

nameSupply :: [Name]
nameSupply = [str2tyvarName (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]