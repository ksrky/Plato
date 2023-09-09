{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.Tc (checkSigma, inferSigma, tcBinds) where

import Control.Exception.Safe (MonadCatch, MonadThrow, catches)
import Control.Monad (forM, unless, void, zipWithM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (local), asks)
import Data.Foldable qualified as Foldable
import Data.Graph
import Data.IORef (IORef)
import Data.Set qualified as S
import GHC.Stack
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Error
import Plato.Typing.Kc
import Plato.Typing.Misc
import Plato.Typing.PatTrans
import Plato.Typing.Tc.Coercion
import Plato.Typing.Tc.InstGen
import Plato.Typing.Tc.SubsCheck
import Plato.Typing.Tc.Unify
import Plato.Typing.Zonking

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
        forall e m.
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LPat ->
        Expected Sigma ->
        m (LPat, [(Ident, Sigma)])
tcPat (L sp pat) exp_ty = tcPat' pat exp_ty
    where
        tcPat' :: Pat -> Expected Sigma -> m (LPat, [(Ident, Sigma)])
        tcPat' WildP _ = return (L sp WildP, [])
        tcPat' (VarP var) (Infer ref) = do
                var_ty <- newTyVar
                writeMIORef ref var_ty
                return (L sp (VarP var), [(var, var_ty)])
        tcPat' (VarP var) (Check exp_ty) = return (L sp (VarP var), [(var, exp_ty)])
        tcPat' (ConP con pats) exp_ty = do
                (arg_tys, res_ty) <- instDataCon con
                unless (length pats == length arg_tys) $ do
                        throwLocErr sp $
                                hsep ["The constrcutor", squotes $ pretty con, "should have", viaShow (length pats), "arguments"]
                (pats', binds) <- checkPats pats arg_tys
                instPatSigma_ res_ty exp_ty
                return (L sp (ConP con pats'), binds)
        tcPat' (AnnP pat ann_ty) exp_ty = do
                (pat', binds) <- checkPat pat ann_ty
                instPatSigma_ ann_ty exp_ty
                return (pat', binds)
        tcPat' TagP{} _ = unreachable "received TagP"
        instPatSigma_ :: Sigma -> Expected Rho -> m ()
        instPatSigma_ sigma (Check rho) =
                catches (instPatSigma sigma exp_ty) (tcErrorHandler sp sigma rho)
        instPatSigma_ sigma (Infer ref) =
                catches (instPatSigma sigma exp_ty) . tcErrorHandler sp sigma =<< readMIORef ref

instPatSigma ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Sigma ->
        Expected Rho ->
        m ()
instPatSigma pat_ty (Infer ref) = do
        (_, rho) <- instantiate pat_ty
        writeMIORef ref rho
instPatSigma pat_ty (Check rho) = void $ subsCheck pat_ty rho

instDataCon ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadThrow m, MonadIO m) =>
        Ident ->
        m ([Sigma], Tau)
instDataCon con = do
        sigma <- find con =<< asks getTypEnv
        (_, rho) <- instantiate sigma
        return $ splitConstrTy rho

-- | Type checking of Rho
checkRho ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Rho ->
        m (LExpr 'Typed)
checkRho exp ty = tcRho exp (Check ty)

inferRho ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Rho)
inferRho exp = do
        ref <- newMIORef (unreachable "inferRho: empty result")
        (,) <$> tcRho exp (Infer ref) <*> readMIORef ref

tcRho ::
        forall e m.
        (HasCallStack, MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Expected Rho ->
        m (LExpr 'Typed)
tcRho (L sp exp) exp_ty = L sp <$> tcRho' exp exp_ty
    where
        tcRho' :: Expr 'Untyped -> Expected Rho -> m (Expr 'Typed)
        tcRho' (VarE var) exp_ty = do
                sigma <- find var =<< asks getTypEnv
                coer <- instSigma_ sigma exp_ty
                return $ unCoer coer $ VarE var
        tcRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- catches (unifyFun fun_ty) (unifunErrorHandler (getLoc fun) fun_ty)
                arg' <- checkSigma arg arg_ty
                coer <- instSigma_ res_ty exp_ty
                return $ unCoer coer $ AppE' (unLoc fun') (unLoc arg')
        tcRho' (AbsE var Nothing body) (Check exp_ty) = do
                (arg_ty, res_ty) <- catches (unifyFun exp_ty) (unifunErrorHandler sp exp_ty)
                body' <- local (modifyTypEnv $ extend var arg_ty) (checkRho body res_ty)
                return $ AbsE' var arg_ty (unLoc body')
        tcRho' (AbsE var (Just var_ty) body) (Check exp_ty) = do
                (arg_ty, res_ty) <- catches (unifyFun exp_ty) (unifunErrorHandler sp exp_ty)
                coer <- instSigma_ arg_ty (Check var_ty)
                body' <- local (modifyTypEnv $ extend var var_ty) (checkRho body res_ty)
                return $ AbsE' var var_ty (substExpr var (unCoer coer $ VarE var) (unLoc body'))
        tcRho' (AbsE var mbty body) (Infer ref) = do
                var_ty <- maybe newTyVar return mbty
                (body', body_ty) <- local (modifyTypEnv $ extend var var_ty) (inferRho body)
                writeMIORef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ AbsE' var var_ty (unLoc body')
        tcRho' (LetE bnds body) exp_ty = do
                bnds' <- tcBinds bnds
                let sigs = map (\(Bind' idty _) -> idty) (Foldable.toList bnds')
                local (modifyTypEnv $ extendList sigs) $ do
                        body' <- tcRho body exp_ty
                        return $ LetE' bnds' body'
        tcRho' (CaseE test alts) exp_ty = do
                (test', pat_ty) <- inferRho test
                exp_ty' <- zapToMonoType exp_ty
                alts' <- forM alts $ \(pat, body) -> do
                        (pat', binds) <- checkPat pat pat_ty
                        (body', body_ty) <- local (modifyTypEnv $ extendList binds) $ inferRho body
                        coer <- instSigma_ body_ty exp_ty'
                        return (pat', unCoer coer $ unLoc body')
                transCase $ CaseE' (unLoc test') pat_ty alts'
        tcRho' (AnnE exp ann_ty) exp_ty = do
                exp' <- checkSigma exp ann_ty
                coer <- instSigma_ ann_ty exp_ty
                return $ unCoer coer $ unLoc exp'
        tcRho' (ClauseE cls) (Check exp_ty) = checkClausesRho cls exp_ty
        tcRho' (ClauseE cls) (Infer ref) = do
                exp_ty <- newTyVar
                exp' <- checkClausesRho cls exp_ty
                writeMIORef ref exp_ty
                return exp'
        instSigma_ :: Sigma -> Expected Rho -> m Coercion
        instSigma_ sigma (Check rho) =
                catches
                        (instSigma sigma (Check rho))
                        (tcErrorHandler sp sigma rho)
        instSigma_ sigma (Infer ref) =
                catches
                        (instSigma sigma (Infer ref))
                        . tcErrorHandler sp sigma
                        =<< readMIORef ref

zapToMonoType :: (MonadReader e m, HasUniq e, MonadIO m) => Expected Rho -> m (Expected Rho)
zapToMonoType (Check ty) = return $ Check ty
zapToMonoType (Infer ref) = do
        ty <- newTyVar
        writeMIORef ref ty
        return $ Check ty

-- | Type checking of Sigma
inferSigma ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Sigma)
inferSigma exp = do
        (exp', rho) <- inferRho exp
        (qns, sigma) <- generalize rho
        return (unCoer (genTrans qns) <$> exp', sigma)

checkSigma ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Sigma ->
        m (LExpr 'Typed)
checkSigma exp sigma = do
        (coer, fqns, rho) <- skolemise sigma
        exp' <- checkRho exp rho
        env_tys <- getEnvTypes
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> mapM getFreeTvs env_tys)
        let bad_tvs = esc_tvs `S.intersection` S.fromList (map fst fqns)
        unless (null bad_tvs) $ throwLocErr (getLoc exp) "Type not polymorphic enough"
        return $ unCoer (coer <> genTrans fqns) <$> exp'

-- | Type checkinng of Clauses
checkClausesRho ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        Clauses 'Untyped ->
        Rho ->
        m (Expr 'Typed)
checkClausesRho clauses rho = do
        (pat_tys, res_ty) <-
                catches
                        (unifyFuns (length (fst $ head clauses)) rho)
                        (unifunErrorHandler (getLoc clauses) rho)
        clauses' <- forM clauses $ \(pats, body) -> do
                (pats', binds) <- checkPats pats pat_tys
                body' <- local (modifyTypEnv $ extendList binds) $ checkRho body res_ty
                return (pats', unLoc body')
        transClauses pat_tys clauses'

tcBinds ::
        forall e m.
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        SCC (Bind 'Untyped) ->
        m (SCC (Bind 'Typed))
tcBinds (AcyclicSCC (Bind (id, Just ty) exp)) = do
        checkKindStar ty
        exp <- checkSigma exp (unLoc ty)
        return $ AcyclicSCC $ Bind' (id, unLoc ty) (unLoc exp)
tcBinds (AcyclicSCC (Bind (id, Nothing) clauses)) = do
        tv <- newTyVar
        exp <- checkRho clauses tv
        (qns, sigma) <- generalize tv
        checkKindStar =<< zonk (noLoc sigma)
        return $ AcyclicSCC $ Bind' (id, sigma) (unCoer (genTrans qns) (unLoc exp))
tcBinds (CyclicSCC binds) = do
        envbinds <- forM binds $ \(Bind (id, mbty) _) -> case mbty of
                Just ty -> return (id, ty)
                Nothing -> (id,) . noLoc <$> newTyVar
        local (modifyTypEnv $ extendList envbinds) $ do
                bbinds' <- forM binds $ \(Bind (id, mbty) exp) -> case mbty of
                        Just ty -> do
                                checkKindStar ty
                                exp <- checkSigma exp (unLoc ty)
                                return (True, Bind' (id, unLoc ty) (unLoc exp))
                        Nothing -> do
                                tv <- find id =<< asks getTypEnv
                                exp <- checkRho exp tv
                                return (False, Bind' (id, tv) (unLoc exp))
                binds' <- forM bbinds' $ \case
                        (True, b) -> return b
                        (False, Bind' (id, ty) exp) -> do
                                (qns, sigma) <- generalize ty
                                return $ Bind' (id, sigma) (unCoer (genTrans qns) exp)
                return $ CyclicSCC binds'

-- | Instantiation of Sigma
instSigma :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer ref) = do
        (coer, rho) <- instantiate sigma
        writeMIORef ref rho
        return coer