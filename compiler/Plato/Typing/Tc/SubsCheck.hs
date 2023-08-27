module Plato.Typing.Tc.SubsCheck (
        subsCheck,
        subsCheckRho,
        subsCheckFun,
) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader)
import Data.Set qualified as S
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
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
        (coercion1, skol_tvs, rho2) <- skolemise sigma2
        coercion2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList (map fst skol_tvs) `S.intersection` esc_tvs
        unless (null bad_tvs) $
                throwError $
                        hsep
                                [ "Impredicative type:"
                                , pretty sigma1
                                , "cannot be instanciated with"
                                , comma
                                , pretty sigma2
                                ]
        return $ deepskolTrans skol_tvs coercion1 coercion2

-- | Subsumption checking. Coersing sigma to rho.
subsCheckRho ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Sigma ->
        Rho ->
        m Coercion
subsCheckRho sigma1@AllT{} rho2 = do
        (coercion1, rho1) <- instantiate sigma1
        coercion2 <- subsCheckRho rho1 rho2
        return (coercion2 <> coercion1)
subsCheckRho (AppT fun1 arg1) (AppT fun2 arg2) = do
        coer_fun <- subsCheckRho (unLoc fun1) (unLoc fun2)
        coer_arg <- subsCheck (unLoc arg1) (unLoc arg2)
        return (coer_fun <> coer_arg)
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