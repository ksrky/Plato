{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.Tc (
        checkType,
        inferType,
) where

import Control.Exception.Safe (MonadThrow)
import Control.Monad (forM, unless, zipWithM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (ask, local))
import Data.IORef (IORef)
import Data.Set qualified as S
import GHC.Stack
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Monad
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.InstGen
import Plato.Typing.Tc.Unify
import Plato.Typing.Tc.Utils
import Plato.Typing.Zonking

checkType ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        Type ->
        m LExpr
checkType = checkSigma

inferType ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        m (LExpr, Type)
inferType = inferSigma

data Expected a = Infer (IORef a) | Check a

-- | Type check of patterns
checkPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LPat ->
        Rho ->
        m [(Ident, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

inferPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LPat ->
        m ([(Ident, Sigma)], Sigma)
inferPat pat = do
        ref <- newMIORef (error "inferRho: empty result")
        binds <- tcPat pat (Infer ref)
        tc <- readMIORef ref
        return (binds, tc)

tcPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LPat ->
        Expected Sigma ->
        m [(Ident, Sigma)]
tcPat (L _ WildP) _ = return []
tcPat (L _ (VarP var)) (Infer ref) = do
        var_ty <- newTyVar
        writeMIORef ref var_ty
        return [(var, var_ty)]
tcPat (L _ (VarP var)) (Check exp_ty) = return [(var, exp_ty)]
tcPat (L sp (ConP con pats)) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        unless (length pats == length arg_tys) $
                throwLocErr sp $
                        hsep ["The constrcutor", squotes $ pretty con, "should have", viaShow (length pats), "arguments"]
        envs <- zipWithM checkPat pats arg_tys
        _ <- instPatSigma res_ty exp_ty
        return (concat envs)

instPatSigma ::
        (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) =>
        Sigma ->
        Expected Sigma ->
        m Coercion
instPatSigma pat_ty (Infer ref) = writeMIORef ref pat_ty >> return Id
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

instDataCon ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        Ident ->
        m ([Sigma], Tau)
instDataCon con = do
        sigma <- find con =<< getEnv =<< ask
        (_, rho) <- instantiate sigma
        return $ split [] rho
    where
        split :: [Sigma] -> Rho -> ([Sigma], Tau)
        split acc (ArrT sigma rho) = split (unLoc sigma : acc) (unLoc rho)
        split acc tau = (acc, tau)

-- | Type check of Rho
checkRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        Rho ->
        m LExpr
checkRho exp ty = do
        exp' <- tcRho exp (Check ty)
        zonkExpr `traverse` exp'

inferRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        m (LExpr, Rho)
inferRho exp = do
        ref <- newMIORef (error "inferRho: empty result")
        exp' <- tcRho exp (Infer ref)
        exp'' <- zonkExpr `traverse` exp'
        (exp'',) <$> readMIORef ref

tcRho ::
        (HasCallStack, MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        Expected Rho ->
        m LExpr
tcRho (L sp exp) exp_ty = L sp <$> tcRho' exp exp_ty
    where
        tcRho' ::
                (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
                Expr ->
                Expected Rho ->
                m Expr
        tcRho' (VarE var) exp_ty = do
                sigma <- find var =<< getEnv =<< ask
                coercion <- instSigma sigma exp_ty
                return $ coercion @@ VarE var
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- unifyFun sp fun_ty
                arg' <- checkSigma arg arg_ty
                coercion <- instSigma res_ty exp_ty
                return $ coercion @@ AppE fun' arg'
        tcRho' (AbsE var _ body) (Check exp_ty) = do
                (var_ty, body_ty) <- unifyFun sp exp_ty
                body' <- local (modifyEnv $ extend var var_ty) (checkRho body body_ty)
                return $ AbsE var (Just var_ty) body'
        tcRho' (AbsE var _ body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- local (modifyEnv $ extend var var_ty) (inferRho body)
                writeMIORef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsE var (Just var_ty) body'
        {-tcRho' (AbsE pat _ body) (Infer ref) = do
                (binds, pat_ty) <- inferPat pat
                (body', body_ty) <- local (modifyEnv $ extendList binds) (inferRho body)
                writeMIORef ref (ArrT (noLoc pat_ty) (noLoc body_ty))
                return $ AbsE pat (Just pat_ty) body'
        tcRho' (AbsE pat _ body) (Check exp_ty) = do
                (arg_ty, res_ty) <- unifyFun sp exp_ty
                binds <- checkPat pat arg_ty
                body' <- local (modifyEnv $ extendList binds) (checkRho body res_ty)
                return $ AbsE pat (Just arg_ty) body'-}
        tcRho' (LetE bnds sigs body) exp_ty = local (modifyEnv $ extendList sigs) $ do
                bnds' <- forM bnds $ \(id, exp) -> do
                        ty <- find id =<< getEnv =<< ask
                        exp' <- checkSigma exp ty
                        return (id, exp')
                body' <- tcRho body exp_ty
                return $ LetE bnds' sigs body'
        tcRho' ClauseE{} _ = undefined
        tcRho' TAppE{} _ = unreachable "TypeCheck.Typ.tcRho"
        tcRho' TAbsE{} _ = unreachable "TypeCheck.Typ.tcRho"

-- | Type check of Sigma
inferSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        m (LExpr, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (tvs, sigma) <- generalize rho
        exp'' <- zonkExpr `traverse` exp'
        return ((genTrans tvs @@) <$> exp'', sigma)

checkSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadThrow m) =>
        LExpr ->
        Sigma ->
        m LExpr
checkSigma exp sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ throwError "Type not polymorphic enough"
        return $ (\e -> coercion @@ genTrans skol_tvs @@ e) <$> exp'

-- | Subsumption checking
subsCheck :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Sigma -> m Coercion
subsCheck sigma1 sigma2 = do
        (coercion1, skol_tvs, rho2) <- skolemise sigma2
        coercion2 <- subsCheckRho sigma1 rho2
        esc_tvs <- S.union <$> getFreeTvs sigma1 <*> getFreeTvs sigma2
        let bad_tvs = S.fromList (map fst skol_tvs) `S.intersection` esc_tvs
        unless (null bad_tvs) $ throwError $ hsep ["Subsumption check failed: ", pretty sigma1 <> comma, pretty sigma2]
        return $ deepskolTrans skol_tvs coercion1 coercion2

subsCheckRho :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Rho -> m Coercion
subsCheckRho sigma1@AllT{} rho2 = do
        (coercion1, rho1) <- instantiate sigma1
        coercion2 <- subsCheckRho rho1 rho2
        return (coercion2 >.> coercion1)
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun NoSpan rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun NoSpan rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = do
        unify NoSpan tau1 tau2
        return Id

subsCheckFun :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Rho -> Sigma -> Rho -> m Coercion
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        funTrans a2 co_arg co_res

-- | Instantiation of Sigma
instSigma :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeMIORef r rho
        return coercion