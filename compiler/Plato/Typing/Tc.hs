{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Tc (
        checkType,
        inferType,
        checkClauses,
) where

import Control.Exception.Safe (Handler (Handler), MonadCatch, MonadThrow, catches)
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
import Plato.Typing.ElabClause
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Monad
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.InstGen
import Plato.Typing.Tc.SubsCheck
import Plato.Typing.Tc.Unify
import Plato.Typing.Tc.Utils
import Plato.Typing.Zonking

checkType ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        Type ->
        m (LExpr 'TcDone)
checkType = checkSigma

inferType ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        m (LExpr 'TcDone, Type)
inferType = inferSigma

data Expected a = Infer (IORef a) | Check a

catchesTcErr :: MonadCatch m => Span -> m a -> m a
catchesTcErr sp m =
        catches
                m
                [ Handler $ \(UnificationError tau_actual tau_exp) ->
                        throwLocErr sp $
                                vsep
                                        [ "Couldn't match type."
                                        , "Expected type:" <+> pretty tau_exp
                                        , "  Actual type:" <+> pretty tau_actual
                                        ]
                , Handler $ \(InfiniteTypeError ty1 ty2) ->
                        throwLocErr sp $
                                hsep ["Infinite type:", squotes $ pretty ty1, "~", squotes $ pretty ty2]
                ]

-- | Type checking of patterns
checkPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LPat ->
        Rho ->
        m [(Ident, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

tcPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
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
        subst <- concat <$> zipWithM checkPat pats arg_tys
        _ <- catchesTcErr sp (instPatSigma res_ty exp_ty)
        return subst

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
        sigma <- zonkType =<< find con =<< getEnv =<< ask
        (_, rho) <- instantiate sigma
        return $ split [] rho
    where
        split :: [Sigma] -> Rho -> ([Sigma], Tau)
        split acc (ArrT sigma rho) = split (unLoc sigma : acc) (unLoc rho)
        split acc tau = (reverse acc, tau)

-- | Type checking of Rho
checkRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        Rho ->
        m (LExpr 'TcDone)
checkRho exp ty = do
        exp' <- tcRho exp (Check ty)
        zonkExpr `traverse` exp'

inferRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        m (LExpr 'TcDone, Rho)
inferRho exp = do
        ref <- newMIORef (unreachable "inferRho: empty result")
        exp' <- tcRho exp (Infer ref)
        exp'' <- zonkExpr `traverse` exp'
        (exp'',) <$> readMIORef ref

tcRho ::
        forall ctx m.
        (HasCallStack, MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        Expected Rho ->
        m (LExpr 'TcDone)
tcRho (L sp exp) exp_ty = L sp <$> tcRho' exp exp_ty
    where
        tcRho' :: Expr 'TcUndone -> Expected Rho -> m (Expr 'TcDone)
        tcRho' (VarE var) exp_ty = do
                sigma <- zonkType =<< find var =<< getEnv =<< ask
                coercion <- catchesTcErr sp (instSigma sigma exp_ty)
                return $ coercion .> VarE var
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- catchesTcErr sp (unifyFun fun_ty)
                arg' <- checkSigma arg arg_ty
                coercion <- catchesTcErr sp (instSigma res_ty exp_ty)
                return $ coercion .> AppE fun' arg'
        tcRho' (AbsE var body) (Check exp_ty) = do
                (var_ty, body_ty) <- catchesTcErr sp (unifyFun exp_ty)
                body' <- local (modifyEnv $ extend var var_ty) (checkRho body body_ty)
                return $ AbsEok var var_ty (unLoc body')
        tcRho' (AbsE var body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- local (modifyEnv $ extend var var_ty) (inferRho body)
                writeMIORef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsEok var var_ty (unLoc body')
        tcRho' (LetE bnds spcs body) exp_ty = local (modifyEnv $ extendList spcs) $ do
                bnds' <- forM bnds $ \(id, clauses) -> do
                        sigma <- zonkType =<< find id =<< getEnv =<< ask
                        exp' <- checkClauses clauses sigma
                        return (id, exp')
                body' <- tcRho body exp_ty
                mapM_ (\(_, ty) -> checkKindStar ty) spcs
                return $ LetEok bnds' spcs body'
        tcRho' (CaseE match alts) exp_ty = do
                (match', match_ty) <- inferRho match
                alts' <- forM alts $ \(pat, body) -> do
                        subst <- checkPat pat match_ty
                        (body', body_ty) <- local (modifyEnv $ extendList subst) $ inferRho body
                        coer <- instSigma body_ty exp_ty
                        return (pat, (coer .>) <$> body')
                return $ CaseEok match' match_ty alts'

-- | Type check of Sigma
inferSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        m (LExpr 'TcDone, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (tvs, sigma) <- generalize rho
        exp'' <- zonkExpr `traverse` exp'
        return ((genTrans tvs .>) <$> exp'', sigma)

checkSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        LExpr 'TcUndone ->
        Sigma ->
        m (LExpr 'TcDone)
checkSigma exp sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ throwError "Type not polymorphic enough"
        return $ (\e -> coercion .> genTrans skol_tvs .> e) <$> exp'

-- | Check clauses
checkClauses ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, HasConEnv ctx, MonadIO m, MonadCatch m) =>
        [Clause 'TcUndone] ->
        Sigma ->
        m (LExpr 'TcDone)
checkClauses clauses sigma_ty = do
        (coer, skol_tvs, rho_ty) <- skolemise sigma_ty
        (pat_tys, res_ty) <- unifyFuns (length (fst $ head clauses)) rho_ty
        -- let (pat_tys, res_ty) = split [] rho_ty
        clauses' <- forM clauses $ \(pats, body) -> do
                subst <- concat <$> zipWithM checkPat pats pat_tys
                body' <- local (modifyEnv $ extendList subst) $ checkSigma body res_ty
                return (pats, body')
        exp <- elabClauses pat_tys clauses'
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma_ty <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ throwError "Type not polymorphic enough"
        return $ (\e -> coer .> genTrans skol_tvs .> e) <$> exp

-- | Instantiation of Sigma
instSigma :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeMIORef r rho
        return coercion