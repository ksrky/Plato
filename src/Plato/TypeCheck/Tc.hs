{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.TypeCheck.Tc where

import Control.Monad.Reader
import qualified Data.Set as S

import Data.IORef
import Prettyprinter

import Control.Exception.Safe
import Plato.Syntax.Typing
import Plato.TypeCheck.InstGen
import Plato.TypeCheck.Monad
import Plato.TypeCheck.Translate
import Plato.TypeCheck.Unify
import Plato.TypeCheck.Utils
import Plato.Types.Error
import Plato.Types.Location

checkType :: (MonadThrow m, MonadIO m) => LExpr -> Type -> m LExpr
checkType e ty = runTc (checkSigma e ty) =<< emptyEnv

inferType :: (MonadThrow m, MonadIO m) => LExpr -> m (LExpr, Type)
inferType e = runTc (inferSigma e) =<< emptyEnv

data Expected a = Infer (IORef a) | Check a

-- | Type check of Rho
checkRho :: (MonadIO m, MonadThrow m) => LExpr -> Rho -> Tc m LExpr
checkRho exp ty = do
        exp' <- tcRho exp (Check ty)
        zonkExpr `traverse` exp'

inferRho :: (MonadIO m, MonadThrow m) => LExpr -> Tc m (LExpr, Rho)
inferRho exp = do
        ref <- newTcRef (error "inferRho: empty result")
        exp' <- tcRho exp (Infer ref)
        exp'' <- zonkExpr `traverse` exp'
        (exp'',) <$> readTcRef ref

tcRho :: (MonadIO m, MonadThrow m) => LExpr -> Expected Rho -> Tc m LExpr
tcRho (L sp exp) exp_ty = writeErrLoc sp >> L sp <$> tcRho' exp exp_ty
    where
        tcRho' :: (MonadIO m, MonadThrow m) => Expr -> Expected Rho -> Tc m Expr
        tcRho' (VarE n) exp_ty = do
                sigma <- lookupEnv n
                coercion <- instSigma sigma exp_ty
                return $ coercion @@ VarE n
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- unifyFun fun_ty
                arg' <- checkSigma arg arg_ty
                coercion <- instSigma res_ty exp_ty
                return $ coercion @@ AppE fun' arg'
        tcRho' (AbsE var _ body) (Check exp_ty) = do
                (var_ty, body_ty) <- unifyFun exp_ty
                body' <- extendEnv var var_ty (checkRho body body_ty)
                return $ AbsE var (Just var_ty) body'
        tcRho' (AbsE var _ body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- extendEnv var var_ty (inferRho body)
                writeTcRef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsE var (Just var_ty) body'
        tcRho' (LetE decs body) exp_ty = do
                let binds = [(var, ty) | FuncD var _ ty <- decs]
                decs' <- forM decs $ \(FuncD var exp ann_ty) -> do
                        exp' <- extendEnvList binds $ checkSigma exp ann_ty
                        return $ FuncD var exp' ann_ty
                body' <- extendEnvList binds (tcRho body exp_ty)
                return $ LetE decs' body'
        tcRho' _ _ = unreachable "TypeCheck.Tc.tcRho"

-- | Type check of Sigma
inferSigma :: (MonadThrow m, MonadIO m) => LExpr -> Tc m (LExpr, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (tvs, sigma) <- generalize rho
        exp'' <- zonkExpr `traverse` exp' -- reduce TyMeta
        return ((genTrans tvs @@) <$> exp'', sigma)

checkSigma :: (MonadIO m, MonadThrow m) => LExpr -> Sigma -> Tc m LExpr
checkSigma exp sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ lift $ throwUnexpErr "Type not polymorphic enough"
        return $ (\e -> coercion @@ genTrans skol_tvs @@ e) <$> exp'

-- | Subsumption checking
subsCheck :: (MonadIO m, MonadThrow m) => Sigma -> Sigma -> Tc m Coercion
subsCheck sigma1 sigma2 = do
        (coercion1, skol_tvs, rho2) <- skolemise sigma2
        coercion2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList (map fst skol_tvs) `S.intersection` esc_tvs
        unless (null bad_tvs) $ lift $ throwUnexpErr $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ deepskolTrans skol_tvs coercion1 coercion2

subsCheckRho :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Tc m Coercion
subsCheckRho sigma1@AllT{} rho2 = do
        (coercion1, rho1) <- instantiate sigma1
        coercion2 <- subsCheckRho rho1 rho2
        return (coercion2 >.> coercion1)
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return Id

subsCheckFun :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Sigma -> Rho -> Tc m Coercion
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return $ funTrans a2 co_arg co_res

-- | Instantiation of Sigma
instSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> Tc m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeTcRef r rho
        return coercion