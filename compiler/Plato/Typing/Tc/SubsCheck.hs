module Plato.Typing.Tc.SubsCheck (
        subsCheck,
        subsCheckRho,
        subsCheckFun,
) where

import Control.Exception.Safe (MonadThrow, throw)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader)
import Data.Set qualified as S

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Error
import Plato.Typing.Misc
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.InstGen
import Plato.Typing.Tc.Unify

{- | Subsumption checking.
  Checks whether sigma1 is subsumption of (more polymorphic than) sigma2
  and returns coercion of sigma1 to sigma2.
-}
subsCheck ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Sigma ->
        Sigma ->
        m Coercion
subsCheck sigma1 sigma2 = do
        (coer1, qns, rho2) <- skolemise sigma2
        coer2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList (map fst qns) `S.intersection` esc_tvs
        unless (null bad_tvs) $ throw SubsCheckFail
        return $ deepskolTrans qns coer1 coer2

-- | Subsumption checking. Coersing sigma to rho.
subsCheckRho ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Sigma ->
        Rho ->
        m Coercion
subsCheckRho sigma1@AllT{} rho2 = do
        (coer1, rho1) <- instantiate sigma1
        coer2 <- subsCheckRho rho1 rho2
        return (coer2 <> coer1)
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return mempty

subsCheckFun ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Sigma ->
        Rho ->
        Sigma ->
        Rho ->
        m Coercion
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        funTrans a2 co_arg co_res