{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing.Tc (checkType, inferType, checkDefn, inferDefn, tcBinds) where

import Control.Exception.Safe (MonadCatch, MonadThrow, catches)
import Control.Monad (forM, unless, void, zipWithM, (<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (local), asks)
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

checkType ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        Type ->
        m (LExpr 'Typed)
checkType = (zonk <=<) . checkSigma

inferType ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LExpr 'Untyped ->
        m (LExpr 'Typed, Type)
inferType = zonk <=< inferSigma

tcBinds ::
        forall e m.
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [Bind 'Untyped] ->
        m [Bind 'Typed]
tcBinds [Bind (id, Just ty) clauses] = do
        checkKindStar ty
        exp <- checkDefn clauses (unLoc ty)
        return [Bind' (id, unLoc ty) exp]
tcBinds [Bind (id, Nothing) clauses] = do
        tv <- newTyVar
        exp <- tcClauses clauses tv
        (qns, sigma) <- generalize tv
        checkKindStar =<< zonk (noLoc sigma)
        return [Bind' (id, sigma) (unCoer (genTrans qns) exp)]
tcBinds binds = do
        envbinds <- forM binds $ \(Bind (id, mbty) _) -> case mbty of
                Just ty -> return (id, ty)
                Nothing -> (id,) . noLoc <$> newTyVar
        local (modifyTypEnv $ extendList envbinds) $ do
                binds' <- forM binds $ \(Bind (id, mbty) clauses) -> case mbty of
                        Just ty -> do
                                checkKindStar ty
                                Bind' (id, unLoc ty) <$> checkDefn clauses (unLoc ty)
                        Nothing -> do
                                tv <- find id =<< asks getTypEnv
                                exp <- tcClauses clauses tv
                                return $ Bind' (id, tv) exp
                zipWithM
                        ( \b@(Bind' (id, ty) exp) (Bind (_, mb) _) -> case mb of
                                Nothing -> do
                                        (qns, sigma) <- generalize ty
                                        return $ Bind' (id, sigma) (unCoer (genTrans qns) exp)
                                _ -> return b
                        )
                        binds'
                        binds

data Expected a = Infer (IORef a) | Check a

instance Show a => Show (Expected a) where
        show Infer{} = "{tyref}"
        show (Check ty) = show ty

checkPats ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [LPat] ->
        [Rho] ->
        m [(Ident, Sigma)]
checkPats pats pat_tys = do
        binds <- zipWithM checkPat pats pat_tys
        return $ concat binds

-- | Type checking of patterns
checkPat ::
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LPat ->
        Rho ->
        m [(Ident, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

tcPat ::
        forall e m.
        (MonadReader e m, HasTypEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        LPat ->
        Expected Sigma ->
        m [(Ident, Sigma)]
tcPat (L sp pat) exp_ty = tcPat' pat exp_ty
    where
        tcPat' :: Pat -> Expected Sigma -> m [(Ident, Sigma)]
        tcPat' WildP _ = return []
        tcPat' (VarP var) (Infer ref) = do
                var_ty <- newTyVar
                writeMIORef ref var_ty
                return [(var, var_ty)]
        tcPat' (VarP var) (Check exp_ty) = return [(var, exp_ty)]
        tcPat' (ConP con pats) exp_ty = do
                (arg_tys, res_ty) <- instDataCon con
                unless (length pats == length arg_tys) $ do
                        throwLocErr sp $
                                hsep ["The constrcutor", squotes $ pretty con, "should have", viaShow (length pats), "arguments"]
                binds <- checkPats pats arg_tys
                instPatSigma_ res_ty exp_ty
                return binds
        tcPat' (AnnP pat ann_ty) exp_ty = do
                binds <- checkPat pat ann_ty
                instPatSigma_ ann_ty exp_ty
                return binds
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
                (arg_ty, res_ty) <- unifyFun_ fun_ty
                arg' <- checkSigma arg arg_ty
                coer <- instSigma_ res_ty exp_ty
                return $ unCoer coer $ AppE' (unLoc fun') (unLoc arg')
        tcRho' (AbsE var Nothing body) (Check exp_ty) = do
                (arg_ty, res_ty) <- unifyFun_ exp_ty
                body' <- local (modifyTypEnv $ extend var arg_ty) (checkRho body res_ty)
                return $ AbsE' var arg_ty (unLoc body')
        tcRho' (AbsE var (Just var_ty) body) (Check exp_ty) = do
                (arg_ty, res_ty) <- unifyFun_ exp_ty
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
                let sigs = map (\(Bind' idty _) -> idty) bnds'
                local (modifyTypEnv $ extendList sigs) $ do
                        body' <- tcRho body exp_ty
                        return $ LetE' bnds' body'
        tcRho' (CaseE test alts) exp_ty = do
                (test', pat_ty) <- inferRho test
                exp_ty' <- zapToMonoType exp_ty
                alts' <- forM alts $ \(pat, body) -> do
                        binds <- checkPat pat pat_ty
                        (body', body_ty) <- local (modifyTypEnv $ extendList binds) $ inferRho body
                        coer <- instSigma_ body_ty exp_ty'
                        return (pat, unCoer coer $ unLoc body')
                transCase $ CaseE' (unLoc test') pat_ty alts'
        tcRho' (AnnE exp ann_ty) exp_ty = do
                exp' <- checkSigma exp ann_ty
                coer <- instSigma_ ann_ty exp_ty
                return $ unCoer coer $ unLoc exp'
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
        unifyFun_ :: Rho -> m (Sigma, Rho)
        unifyFun_ rho = catches (unifyFun rho) (unifunErrorHandler sp rho)

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
tcClauses ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [Clause 'Untyped] ->
        Rho ->
        m (Expr 'Typed)
tcClauses clauses rho = do
        (pat_tys, res_ty) <-
                catches
                        (unifyFuns (length (fst $ head clauses)) rho)
                        (unifunErrorHandler (getLoc clauses) rho)
        clauses' <- forM clauses $ \(pats, body) -> do
                binds <- checkPats pats pat_tys
                body' <- local (modifyTypEnv $ extendList binds) $ checkRho body res_ty
                return (pats, unLoc body')
        transClauses pat_tys clauses'

checkDefn ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [Clause 'Untyped] ->
        Sigma ->
        m (Expr 'Typed)
checkDefn clauses sigma = do
        (coer, fqns, rho) <- skolemise sigma
        exp <- tcClauses clauses rho
        esc_tvs <- S.union <$> getFreeTvs sigma <*> (mconcat <$> (mapM getFreeTvs =<< getEnvTypes))
        let bad_tvs = esc_tvs `S.intersection` S.fromList (map fst fqns)
        unless (null bad_tvs) $ throwError "Type not polymorphic enough"
        return $ unCoer (coer <> genTrans fqns) exp

inferDefn ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadIO m, MonadCatch m) =>
        [Clause 'Untyped] ->
        m (Expr 'Typed, Sigma)
inferDefn clauses = do
        tv <- newTyVar
        exp <- tcClauses clauses tv
        (qns, sigma) <- generalize tv
        return (unCoer (genTrans qns) exp, sigma)

-- | Instantiation of Sigma
instSigma :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> m Coercion
instSigma sigma (Check rho) = subsCheckRho sigma rho
instSigma sigma (Infer r) = do
        (coer, rho) <- instantiate sigma
        writeMIORef r rho
        return coer