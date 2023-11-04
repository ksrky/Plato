module Plato.Typing.Tc.InstGen (generalize, instantiate, skolemise) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Set                   qualified as S

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
    tys <- mapM renewTyVar qns
    tau' <- substTvs (map fst qns) tys (unLoc tau)
    return (instTrans tys, tau')
instantiate ty = return (mempty, ty)

-- | Skolemisation
skolemise :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> m (Coercion, [Quant], Rho)
skolemise (AllT qns rho) = do
    qns1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newFreeTv tv) qns
    (coer, qns2, rho') <- skolemise =<< substTvs (map fst qns) (map (VarT . fst) qns1) (unLoc rho)
    -- ftvs <- getFreeTvs rho'
    -- let qns1' =  filter (\(tv, _) -> tv `S.member` ftvs) qns1
    return (prpolyTrans qns1 coer, qns1 ++ qns2, rho')
skolemise (ArrT arg_ty res_ty) = do
    (coer, qns, res_ty') <- skolemise (unLoc res_ty)
    coer' <- prfunTrans qns (unLoc arg_ty) coer
    return (coer', qns, ArrT arg_ty (noLoc res_ty'))
skolemise rho = return (mempty, [], rho)

-- | Generalization
generalize :: (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m) => Rho -> m ([Quant], Sigma)
generalize ty = do
    env_tvs <- mapM getMetaTvs =<< getEnvTypes
    res_tvs <- getMetaTvs ty
    let all_tvs = res_tvs `S.difference` mconcat env_tvs
    quantify (S.toList all_tvs) ty

quantify ::
    (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m) => [MetaTv] -> Rho -> m ([Quant], Sigma)
quantify [] rho = return ([], rho)
quantify tvs rho = do
    names <- freshTvNames
    qns <- zipWithM newQuant names tvs
    return (qns, AllT qns (noLoc rho))

newQuant :: (MonadReader e m, HasUniq e, MonadIO m) => Name -> MetaTv -> m Quant
newQuant _ mtv@(MetaTv{meta_quant =Just (tv, kn)}) = do
    tv' <- newFreeTv tv
    writeMetaTv mtv (VarT tv')
    return (tv', kn)
newQuant name mtv@(MetaTv{meta_quant =Nothing}) = do
    tv <- BoundTv <$> freshIdent name
    writeMetaTv mtv (VarT tv)
    kn <- newKnVar
    return (tv, kn)
