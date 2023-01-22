{-# LANGUAGE TupleSections #-}

module Plato.TypeCheck.InstGen where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Set as S

import Plato.Syntax.Typing
import Plato.TypeCheck.Monad
import Plato.TypeCheck.Subst
import Plato.TypeCheck.Translate
import Plato.TypeCheck.Utils
import Plato.Common.Location
import Plato.Common.Name

-- | Instantiation
instantiate :: MonadIO m => Sigma -> Tc m (Coercion, Rho)
instantiate (AllT tvs tau) = do
        tys <- mapM (const newTyVar) tvs
        return (instTrans tys, subst (map fst tvs) tys (unLoc tau))
instantiate ty = return (Id, ty)

skolemise :: MonadIO m => Sigma -> Tc m (Coercion, [(TyVar, Maybe Kind)], Rho)
skolemise (AllT tvs rho) = do
        sks1 <- mapM (\(tv, mbkn) -> (,mbkn) <$> newSkolemTyVar tv) tvs
        (coercion, sks2, ty') <- skolemise (subst (map fst tvs) (map (VarT . fst) sks1) (unLoc rho))
        return (prpolyTrans sks1 coercion, sks1 ++ sks2, ty')
skolemise (ArrT arg_ty res_ty) = do
        (coercion, sks, res_ty') <- skolemise (unLoc res_ty)
        return (prfunTrans sks (unLoc arg_ty) coercion, sks, ArrT arg_ty (noLoc res_ty'))
skolemise ty = return (Id, [], ty)

-- | Generalization
generalize :: MonadIO m => Rho -> Tc m ([TyVar], Sigma)
generalize ty = do
        env_tvs <- mapM getMetaTvs =<< getEnvTypes
        res_tvs <- getMetaTvs ty
        let all_tvs = res_tvs `S.difference` mconcat env_tvs
        quantify (S.toList all_tvs) ty

quantify :: MonadIO m => [MetaTv] -> Rho -> Tc m ([TyVar], Sigma)
quantify [] ty = return ([], ty)
quantify tvs ty = do
        let new_bndrs = take (length tvs) allBinders
        zipWithM_ writeMetaTv tvs (map VarT new_bndrs)
        ty' <- zonkType ty
        return (new_bndrs, AllT (map (,Nothing) new_bndrs) (noLoc ty'))

allBinders :: [TyVar]
allBinders =
        [BoundTv $ noLoc $ str2tyvarName [x] | x <- ['a' .. 'z']]
        ++ [BoundTv $ noLoc $ str2tyvarName (x : show i) | i <- [1 :: Integer ..], x <- ['a' .. 'z']]