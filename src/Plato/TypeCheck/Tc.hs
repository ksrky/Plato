{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.TypeCheck.Tc where

import Control.Monad.Reader
import qualified Data.Set as S

import Data.IORef
import Prettyprinter

import Control.Exception.Safe
import Plato.Common.Error
import Plato.Common.Location
import Plato.Syntax.Typing
import Plato.TypeCheck.InstGen
import Plato.TypeCheck.Translate
import Plato.TypeCheck.Unify
import Plato.TypeCheck.Utils
import Plato.Typing.Env
import Plato.Typing.Monad

checkType :: (MonadThrow m, MonadIO m) => LExpr -> Type -> Typ m LExpr
checkType = checkSigma

inferType :: (MonadThrow m, MonadIO m) => LExpr -> Typ m (LExpr, Type)
inferType = inferSigma

data Expected a = Infer (IORef a) | Check a

-- | Type check of patterns
checkPat :: (MonadThrow m, MonadIO m) => LPat -> Rho -> Typ m [(LName, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

inferPat :: (MonadThrow m, MonadIO m) => LPat -> Typ m ([(LName, Sigma)], Sigma)
inferPat pat = do
        ref <- newTypRef (error "inferRho: empty result")
        binds <- tcPat pat (Infer ref)
        tc <- readTypRef ref
        return (binds, tc)

tcPat :: (MonadThrow m, MonadIO m) => LPat -> Expected Sigma -> Typ m [(LName, Sigma)]
tcPat (L _ WildP) _ = return []
tcPat (L _ (VarP var)) (Infer ref) = do
        var_ty <- newTyVar
        writeTypRef ref var_ty
        return [(var, var_ty)]
tcPat (L _ (VarP var)) (Check exp_ty) = return [(var, exp_ty)]
tcPat (L sp (ConP con pats)) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        unless (length pats == length arg_tys) $
                throwTyp sp $ hsep ["The constrcutor", squotes $ pretty con, "should have", viaShow (length pats), "arguments"]
        envs <- zipWithM checkPat pats arg_tys
        _ <- instPatSigma res_ty exp_ty
        return (concat envs)

instPatSigma :: (MonadThrow m, MonadIO m) => Sigma -> Expected Sigma -> Typ m Coercion
instPatSigma pat_ty (Infer ref) = writeTypRef ref pat_ty >> return Id
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

instDataCon :: (MonadThrow m, MonadIO m) => LTypName -> Typ m ([Sigma], Tau)
instDataCon con = do
        sigma <- asksM $ lookupInEnv con
        (_, rho) <- instantiate sigma
        return $ split [] rho
    where
        split :: [Sigma] -> Rho -> ([Sigma], Tau)
        split acc (ArrT sigma rho) = split (unLoc sigma : acc) (unLoc rho)
        split acc tau = (acc, tau)

-- | Type check of Rho
checkRho :: (MonadIO m, MonadThrow m) => LExpr -> Rho -> Typ m LExpr
checkRho exp ty = do
        exp' <- tcRho exp (Check ty)
        zonkExpr `traverse` exp'

inferRho :: (MonadIO m, MonadThrow m) => LExpr -> Typ m (LExpr, Rho)
inferRho exp = do
        ref <- newTypRef (error "inferRho: empty result")
        exp' <- tcRho exp (Infer ref)
        exp'' <- zonkExpr `traverse` exp'
        (exp'',) <$> readTypRef ref

tcRho :: (MonadIO m, MonadThrow m) => LExpr -> Expected Rho -> Typ m LExpr
tcRho (L sp exp) exp_ty = writeErrLoc sp >> L sp <$> tcRho' exp exp_ty
    where
        tcRho' :: (MonadIO m, MonadThrow m) => Expr -> Expected Rho -> Typ m Expr
        tcRho' (VarE var) exp_ty = do
                sigma <- asksM $ lookupInEnv var
                coercion <- instSigma sigma exp_ty
                return $ coercion @@ VarE var
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- unifyFun fun_ty
                arg' <- checkSigma arg arg_ty
                coercion <- instSigma res_ty exp_ty
                return $ coercion @@ AppE fun' arg'
        tcRho' (AbsE var _ body) (Check exp_ty) = do
                (var_ty, body_ty) <- unifyFun exp_ty
                body' <- local (extendEnv var var_ty) (checkRho body body_ty)
                return $ AbsE var (Just var_ty) body'
        tcRho' (AbsE var _ body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- local (extendEnv var var_ty) (inferRho body)
                writeTypRef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsE var (Just var_ty) body'
        tcRho' (PAbsE pat _ body) (Infer ref) = do
                (binds, pat_ty) <- inferPat pat
                (body', body_ty) <- local (extendEnvList binds) (inferRho body)
                writeTypRef ref (ArrT (noLoc pat_ty) (noLoc body_ty))
                return $ PAbsE pat (Just pat_ty) body'
        tcRho' (PAbsE pat _ body) (Check exp_ty) = do
                (arg_ty, res_ty) <- unifyFun exp_ty
                binds <- checkPat pat arg_ty
                body' <- local (extendEnvList binds) (checkRho body res_ty)
                return $ PAbsE pat (Just arg_ty) body'
        tcRho' (LetE binds@(Binds _ sigs) body) exp_ty = do
                binds' <- tcBinds binds
                body' <- local (extendEnvList sigs) (tcRho body exp_ty)
                return $ LetE binds' body'
        tcRho' _ _ = unreachable "TypeCheck.Typ.tcRho"

-- | Type check of Sigma
inferSigma :: (MonadThrow m, MonadIO m) => LExpr -> Typ m (LExpr, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (tvs, sigma) <- generalize rho
        exp'' <- zonkExpr `traverse` exp'
        return ((genTrans tvs @@) <$> exp'', sigma)

checkSigma :: (MonadIO m, MonadThrow m) => LExpr -> Sigma -> Typ m LExpr
checkSigma exp sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ lift $ throwUnexpErr "Type not polymorphic enough"
        return $ (\e -> coercion @@ genTrans skol_tvs @@ e) <$> exp'

-- | Type check of Binders
tcBinds :: (MonadIO m, MonadThrow m) => Binds -> Typ m Binds
tcBinds (Binds binds sigs) = do
        binds' <- forM binds $ \(var, exp) -> do
                ann_ty <- case lookup var sigs of
                        Just ty -> return ty
                        Nothing -> throwTyp (getLoc var) $ hsep ["function", squotes $ pretty var, "lacks signature"]
                exp' <- local (extendEnvList sigs) $ checkSigma exp ann_ty
                return (var, exp')
        return $ Binds binds' sigs

-- | Subsumption checking
subsCheck :: (MonadIO m, MonadThrow m) => Sigma -> Sigma -> Typ m Coercion
subsCheck sigma1 sigma2 = do
        (coercion1, skol_tvs, rho2) <- skolemise sigma2
        coercion2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList (map fst skol_tvs) `S.intersection` esc_tvs
        unless (null bad_tvs) $ lift $ throwUnexpErr $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ deepskolTrans skol_tvs coercion1 coercion2

subsCheckRho :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Typ m Coercion
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

subsCheckFun :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Sigma -> Rho -> Typ m Coercion
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return $ funTrans a2 co_arg co_res

-- | Instantiation of Sigma
instSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> Typ m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeTypRef r rho
        return coercion