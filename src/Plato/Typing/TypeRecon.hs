{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.TypeRecon where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TrMonad
import Plato.Typing.TrTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer as Writer
import Data.IORef
import Data.List

typeRecon :: (MonadIO m, MonadThrow m) => [(Name, Sigma)] -> FuncDecl -> m FuncDecl
typeRecon env (FD var body ty) = runTr env $ do
        (ann_expr, ty') <- inferSigma (cL body $ AnnE body ty)
        let body' = case ann_expr of
                L _ (AnnE e t) -> e
                _ -> unreachable ""
        return (FD var body' (L (getSpan ty) ty'))

typecheck :: (MonadIO m, MonadThrow m) => Located Expr -> Located Type -> Tr m Sigma
typecheck e ty = do
        (_, ty') <- inferSigma (cL e $ AnnE e ty)
        zonkType ty'

data Expected a = Infer (IORef a) | Check a

------------------------------------------
-- tcPat, and its variants
------------------------------------------
-- tmp: no translation for patterns
-- because pattern match is implemented by classifying data consructor's tag.
-- If type application is inserted in ConP, GADT will be available.
checkPat :: (MonadIO m, MonadThrow m) => Pat -> Rho -> Tr m [(Located Name, Sigma)]
checkPat pat ty = trPat pat (Check ty)

inferPat :: (MonadIO m, MonadThrow m) => Pat -> Tr m ([(Located Name, Sigma)], Sigma)
inferPat pat = do
        ref <- newTrRef (error "inferRho: empty result")
        binds <- trPat pat (Infer ref)
        tc <- readTrRef ref
        return (binds, tc)

trPat :: (MonadIO m, MonadThrow m) => Pat -> Expected Sigma -> Tr m [(Located Name, Sigma)]
trPat WildP _ = return []
trPat (VarP v) (Infer ref) = do
        ty <- newTyVar
        writeTrRef ref ty
        return [(v, ty)]
trPat (VarP v) (Check ty) = return [(v, ty)]
trPat (ConP con ps) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        envs <- mapM check_arg (map unLoc ps `zip` arg_tys)
        instPatSigma res_ty exp_ty
        return (concat envs)
    where
        check_arg (p, ty) = checkPat p ty

instPatSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Sigma -> Tr m (Located Expr -> Located Expr)
instPatSigma pat_ty (Infer ref) = writeTrRef ref pat_ty >> return id
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

instDataCon :: (MonadIO m, MonadThrow m) => Located Name -> Tr m ([Sigma], Tau) --tmp: data constructor be in TrEnv
instDataCon con = do
        con_ty <- lookupVar con
        (_, con_ty') <- instantiate con_ty
        return $ go con_ty' []
    where
        go :: Type -> [Type] -> ([Type], Type)
        go (ArrT arg res) acc = go (unLoc res) (unLoc arg : acc)
        go ty acc = (reverse acc, ty)

------------------------------------------
-- trRho, and its variants
------------------------------------------
checkRho :: (MonadIO m, MonadThrow m) => Located Expr -> Rho -> Tr m (Located Expr)
checkRho expr ty = trRho expr (Check ty)

inferRho :: (MonadIO m, MonadThrow m) => Located Expr -> Tr m (Located Expr, Rho)
inferRho expr = do
        ref <- newTrRef (error "inferRho: empty result")
        expr <- trRho expr (Infer ref)
        (expr,) <$> readTrRef ref

trRho :: (MonadIO m, MonadThrow m) => Located Expr -> Expected Rho -> Tr m (Located Expr)
trRho (L sp exp) = trRho' exp
    where
        trRho' :: (MonadIO m, MonadThrow m) => Expr -> Expected Rho -> Tr m (Located Expr)
        trRho' (VarE v) exp_ty = do
                v_sigma <- lookupVar v
                coercion <- instSigma v_sigma exp_ty
                return $ coercion (L sp $ VarE v)
        trRho' (AppE fun arg) exp_ty = do
                (fun', fun_ty) <- inferRho fun
                (arg_ty, res_ty) <- unifyFun fun_ty
                arg' <- checkSigma arg arg_ty
                coercion <- instSigma res_ty exp_ty
                return $ coercion $ L sp $ AppE fun' arg'
        trRho' (AbsE var Nothing body) (Check exp_ty) = do
                (var_ty, body_ty) <- unifyFun exp_ty
                body' <- extendVarEnv (unLoc var) var_ty (checkRho body body_ty)
                return $ L sp $ AbsE var (Just var_ty) body'
        trRho' (AbsE var Nothing body) (Infer ref) = do
                var_ty <- newTyVar
                (body', body_ty) <- extendVarEnv (unLoc var) var_ty (inferRho body)
                writeTrRef ref (ArrT (noLoc var_ty) (noLoc body_ty))
                return $ L sp $ AbsE var (Just var_ty) body'
        trRho' (LetE decs body) exp_ty = do
                (decs', binds) <- execWriterT $
                        forM decs $ \(FD var var_e ann_ty) -> do
                                (var_e', var_ty) <- Writer.lift $ inferSigma (cL var_e $ AnnE var_e ann_ty)
                                let d = FD var var_e' (L (getSpan ann_ty) var_ty)
                                tell ([d], [(unLoc var, var_ty)])
                body' <- extendVarEnvList binds (trRho' (unLoc body) exp_ty)
                return $ L sp $ LetE decs' body'
        trRho' (ProjE exp lab) exp_ty = undefined
        trRho' (RecordE fields) exp_ty = undefined
        trRho' (CaseE match _ alts) (Check exp_ty) = do
                (match', match_ty) <- inferRho match
                alts' <- forM alts $ \(pat, body) -> do
                        checkPat (unLoc pat) match_ty
                        body' <- checkRho body exp_ty
                        return (pat, body')
                return $ L sp $ CaseE match' (Just match_ty) alts'
        trRho' (CaseE match _ alts) (Infer ref) = do
                (match', match_ty) <- inferRho match
                body_tys <- forM alts $ \(pat, body) -> do
                        checkPat (unLoc pat) match_ty
                        inferRho body
                let pairs = filter ((2 ==) . length) $ subsequences body_tys
                forM_ pairs $ \[(e1, ty1), (e2, ty2)] -> do
                        subsCheck ty1 ty2
                        subsCheck ty2 ty1
                return $ L sp $ CaseE match' (Just match_ty) alts
        trRho' TagE{} exp_ty = undefined
        trRho' (AnnE body ann_ty) exp_ty = do
                checkSigma body (unLoc ann_ty)
                coercion <- instSigma (unLoc ann_ty) exp_ty
                return $ L sp $ AnnE (coercion body) ann_ty
        trRho' _ _ = unreachable "TAbsExpr, TAppExpr"

------------------------------------------
-- inferSigma and checkSigma
------------------------------------------
inferSigma :: (MonadIO m, MonadThrow m) => Located Expr -> Tr m (Located Expr, Sigma)
inferSigma e = do
        (expr, exp_ty) <- inferRho e
        env_tys <- getEnvTypes
        env_tvs <- getMetaTyVars env_tys
        res_tvs <- getMetaTyVars [exp_ty]
        let forall_tvs = res_tvs \\ env_tvs
        if null forall_tvs
                then return (expr, exp_ty)
                else (expr,) <$> quantify forall_tvs exp_ty

checkSigma :: (MonadIO m, MonadThrow m) => Located Expr -> Sigma -> Tr m (Located Expr)
checkSigma expr sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        expr' <- checkRho expr rho
        env_tys <- getEnvTypes
        esc_tvs <- getFreeTyVars (sigma : env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "Type not polymorphic enough" --tmp
        return $ coercion expr'

------------------------------------------
--        Subsumption checking          --
------------------------------------------
-- See Technical Appendix p53 for creating coercion terms
subsCheck :: (MonadIO m, MonadThrow m) => Sigma -> Sigma -> Tr m (Located Expr -> Located Expr)
subsCheck sigma1 sigma2 = do
        (co1, skol_tvs, rho2) <- skolemise sigma2
        co2 <- subsCheckRho sigma1 rho2
        esc_tvs <- getFreeTyVars [sigma1, sigma2]
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "Subsumption check failed" --tmp
        if null skol_tvs
                then return id
                else return $ \e -> co1 (cL e $ TAbsE (map tyVarName skol_tvs) (co2 e))

subsCheckRho :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Tr m (Located Expr -> Located Expr)
subsCheckRho sigma1@(AllT tvs _) rho2 = do
        (co1, rho1) <- instantiate sigma1
        co2 <- subsCheckRho rho1 rho2
        return $ \e -> (co2 . co1) $ cL e $ TAppE e (map VarT tvs)
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return id

subsCheckFun :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Sigma -> Rho -> Tr m (Located Expr -> Located Expr)
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return
                ( \f ->
                        cL f $
                                AbsE
                                        (noLoc $ str2varName "x")
                                        (Just a2)
                                        (co_res $ cL f $ AppE f (co_arg (noLoc $ VarE $ noLoc $ str2varName "x")))
                )

instSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> Tr m (Located Expr -> Located Expr)
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
        (coercion, t1') <- instantiate t1
        writeTrRef r t1'
        return coercion
