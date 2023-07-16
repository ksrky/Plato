{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Tc (
        checkType,
        inferType,
        checkClauses,
) where

import Control.Exception.Safe (MonadCatch, MonadThrow, catches)
import Control.Monad (forM, unless, zipWithM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.IORef (IORef)
import Data.Set qualified as S
import GHC.Stack
import Prettyprinter
import System.Log.Logger

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Logger
import Plato.Syntax.Typing
import Plato.Typing.ElabClause
import Plato.Typing.Env
import Plato.Typing.Error
import Plato.Typing.Kc
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.InstGen
import Plato.Typing.Tc.SubsCheck
import Plato.Typing.Tc.Unify
import Plato.Typing.Tc.Utils
import Plato.Typing.Utils
import Plato.Typing.Zonking

checkType ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Type ->
        m (LExpr 'Typed)
checkType = checkSigma

inferType ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Type)
inferType = inferSigma

data Expected a = Infer (IORef a) | Check a

instance Show a => Show (Expected a) where
        show Infer{} = "{tyref}"
        show (Check ty) = show ty

checkPats ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [LPat] ->
        [Rho] ->
        m ([LPat], [(Ident, Sigma)])
checkPats pats pat_tys = do
        (pats', binds) <- unzip <$> zipWithM checkPat pats pat_tys
        return (pats', concat binds)

-- | Type checking of patterns
checkPat ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LPat ->
        Rho ->
        m (LPat, [(Ident, Sigma)])
checkPat pat ty = tcPat pat (Check ty)

tcPat ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LPat ->
        Expected Sigma ->
        m (LPat, [(Ident, Sigma)])
tcPat pat@(L _ WildP) _ = return (pat, [])
tcPat pat@(L sp (VarP var)) (Infer ref) = do
        var_ty <- newTyVar
        writeMIORef ref var_ty
        return (L sp (AnnP pat var_ty), [(var, var_ty)])
tcPat pat@(L sp (VarP var)) (Check exp_ty) =
        return (L sp (AnnP pat exp_ty), [(var, exp_ty)])
tcPat (L sp (ConP con pats)) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        unless (length pats == length arg_tys) $ do
                throwLocErr sp $
                        hsep ["The constrcutor", squotes $ pretty con, "should have", viaShow (length pats), "arguments"]
        (pats', binds) <- checkPats pats arg_tys
        res_ty' <- zonk res_ty -- Note: Argument type might applied to result type
        _ <- apInstSigma sp instPatSigma res_ty' exp_ty
        return (L sp (AnnP (L sp (ConP con pats')) res_ty'), binds)
tcPat (L sp (AnnP pat pat_ty)) exp_ty = do
        (pat', binds) <- checkPat pat pat_ty
        _ <- apInstSigma sp instPatSigma pat_ty exp_ty
        return (L sp (AnnP pat' pat_ty), binds)

instPatSigma ::
        (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) =>
        Sigma ->
        Expected Sigma ->
        m Coercion
instPatSigma pat_ty (Infer ref) = writeMIORef ref pat_ty >> return Id
instPatSigma pat_ty (Check exp_ty) = subsCheck pat_ty exp_ty

instDataCon ::
        (MonadReader ctx m, HasTypEnv ctx, HasUniq ctx, MonadThrow m, MonadIO m) =>
        Ident ->
        m ([Sigma], Tau)
instDataCon con = do
        sigma <- zonk =<< find con =<< asks getTypEnv
        (_, rho) <- instantiate sigma
        return $ splitConstrTy rho

-- | Type checking of Rho
checkRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Rho ->
        m (LExpr 'Typed)
checkRho exp ty = do
        exp' <- tcRho exp (Check ty)
        zonk `traverse` exp'

inferRho ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Rho)
inferRho exp = do
        ref <- newMIORef (unreachable "inferRho: empty result")
        exp' <- tcRho exp (Infer ref)
        exp'' <- zonk `traverse` exp'
        (exp'',) <$> readMIORef ref

tcRho ::
        forall ctx m.
        (HasCallStack, MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Expected Rho ->
        m (LExpr 'Typed)
tcRho (L sp exp) exp_ty = do
        liftIO $ debugM platoLog $ "tcRho: " ++ show exp ++ " : " ++ show exp_ty
        L sp <$> tcRho' exp exp_ty
    where
        tcRho' :: Expr 'Untyped -> Expected Rho -> m (Expr 'Typed)
        tcRho' (VarE var) exp_ty = do
                sigma <- zonk =<< find var =<< asks getTypEnv
                coercion <- apInstSigma sp instSigma sigma exp_ty
                return $ coercion .> VarE var
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                liftIO $ debugM platoLog $ "tcRho': " ++ show fun' ++ " : " ++ show fun_ty
                (arg_ty, res_ty) <- apUnifyFun sp unifyFun fun_ty
                arg' <- checkSigma arg arg_ty
                res_ty' <- zonk res_ty -- Note: Argument type might applied to result type
                coercion <- apInstSigma sp instSigma res_ty' exp_ty
                return $ coercion .> AppE fun' arg'
        tcRho' (AbsE var body) (Check exp_ty) = do
                (var_ty, body_ty) <- apUnifyFun sp unifyFun exp_ty
                body' <- local (modifyTypEnv $ extend var var_ty) (checkRho body body_ty)
                return $ AbsEok var var_ty (unLoc body')
        tcRho' (AbsE var body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- local (modifyTypEnv $ extend var var_ty) (inferRho body)
                writeMIORef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsEok var var_ty (unLoc body')
        tcRho' (LetE bnds spcs body) exp_ty = local (modifyTypEnv $ extendList spcs) $ do
                bnds' <- forM bnds $ \(id, clauses) -> do
                        sigma <- zonk =<< find id =<< asks getTypEnv
                        exp' <- checkClauses clauses sigma
                        return (id, exp')
                body' <- tcRho body exp_ty
                mapM_ (\(_, ty) -> checkKindStar ty) spcs
                return $ LetEok bnds' spcs body'
        tcRho' (CaseE mat alts) exp_ty = do
                (mat', mat_ty) <- inferRho mat
                exp_ty' <- zapToMonoType exp_ty
                alts' <- forM alts $ \(pat, body) -> do
                        (pat', binds) <- checkPat pat mat_ty
                        (body', body_ty) <- local (modifyTypEnv $ extendList binds) $ inferRho body
                        coer <- instSigma body_ty exp_ty'
                        return (pat', (coer .>) <$> body')
                elabCase $ CaseEok mat' mat_ty alts'

zapToMonoType :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Expected Rho -> m (Expected Rho)
zapToMonoType (Check ty) = return $ Check ty
zapToMonoType (Infer ref) = do
        ty <- newTyVar
        writeMIORef ref ty
        return $ Check ty

-- | Type check of Sigma
inferSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (tvs, sigma) <- generalize rho
        exp'' <- zonk `traverse` exp'
        return ((genTrans tvs .>) <$> exp'', sigma)

checkSigma ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Sigma ->
        m (LExpr 'Typed)
checkSigma exp sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ do
                liftIO $ debugM platoLog $ "esc_tvs: " ++ show esc_tvs ++ ", " ++ "skol_tvs: " ++ show skol_tvs
                throwError "Type not polymorphic enough"
        return $ (\e -> coercion .> genTrans skol_tvs .> e) <$> exp'

-- | Check clauses
checkClauses ::
        (MonadReader ctx m, HasTypEnv ctx, HasConEnv ctx, HasUniq ctx, MonadIO m, MonadCatch m) =>
        [Clause 'Untyped] ->
        Sigma ->
        m (LExpr 'Typed)
checkClauses clauses sigma_ty = do
        (coer, skol_tvs, rho_ty) <- skolemise sigma_ty
        (pat_tys, res_ty) <- apUnifyFun (getLoc clauses) (unifyFuns (length (fst $ head clauses))) rho_ty
        clauses' <- forM clauses $ \(pats, body) -> do
                (_, binds) <- checkPats pats pat_tys
                body' <- local (modifyTypEnv $ extendList binds) $ checkSigma body res_ty
                return (pats, body')
        exp <- elabClauses pat_tys clauses'
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma_ty <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = filter (`elem` esc_tvs) (map fst skol_tvs)
        unless (null bad_tvs) $ do
                liftIO $ debugM platoLog $ "esc_tvs: " ++ show esc_tvs ++ ", " ++ "skol_tvs: " ++ show skol_tvs
                throwError "Type not polymorphic enough"
        return $ (\e -> coer .> genTrans skol_tvs .> e) <$> exp

-- | Instantiation of Sigma
instSigma :: (MonadReader ctx m, HasUniq ctx, MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coercion, rho) <- instantiate sigma
        writeMIORef r rho
        return coercion

-----------------------------------------------------------
-- Applying a function with catching errors
-----------------------------------------------------------
apInstSigma ::
        (MonadIO m, MonadCatch m) =>
        Span ->
        (Type -> Expected Type -> m a) ->
        Type ->
        Expected Type ->
        m a
apInstSigma sp inst ty_exp expty@(Check ty_sup) = do
        ty_exp' <- zonk ty_exp
        catches (inst ty_exp expty) (instErrHandler sp ty_exp' ty_sup)
apInstSigma sp inst ty_exp expty@(Infer ref) = do
        ty_exp' <- zonk ty_exp
        ty_sup <- readMIORef ref
        catches (inst ty_exp expty) (instErrHandler sp ty_exp' ty_sup)

apUnifyFun ::
        MonadCatch m =>
        Span ->
        (Rho -> m (a, Rho)) ->
        Rho ->
        m (a, Rho)
apUnifyFun sp unifun rho = catches (unifun rho) (unifunErrHandler sp rho)
