{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.TypeCheck where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TcMonad
import Plato.Typing.TcTypes

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer as Writer
import Data.IORef
import Data.List (subsequences, (\\))

typeCheck :: (MonadIO m, MonadThrow m) => TypTable -> FuncD -> m FuncD
typeCheck env (FuncD var body ty) = runTc env $ do
        (AnnE body' _, ty') <- inferSigma (AnnE body ty)
        body'' <- zonkExpr body'
        ty'' <- zonkType ty'
        return (FuncD var body'' ty'')

typeInfer :: (MonadIO m, MonadThrow m) => TypTable -> Expr -> m (Expr, Sigma)
typeInfer env e = runTc env $ do
        (e', ty') <- inferSigma e
        e'' <- zonkExpr e'
        ty'' <- zonkType ty'
        return (e'', ty'')

isBasicType :: Type -> Bool
isBasicType ConT{} = True
isBasicType (AppT fun res) = isBasicType fun && isBasicType res
isBasicType _ = False

data Expected a = Infer (IORef a) | Check a

------------------------------------------
-- tcPat, and its variants
------------------------------------------
--tmp: no translation for patterns
-- because pattern match is implemented by classifying data consructor's tag.
-- If type application is inserted in ConP, GADT will be available.
checkPat :: (MonadIO m, MonadThrow m) => Pat -> Rho -> Tc m [(LName, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

inferPat :: (MonadIO m, MonadThrow m) => Pat -> Tc m ([(LName, Sigma)], Sigma)
inferPat pat = do
        ref <- newTcRef (error "inferRho: empty result")
        binds <- tcPat pat (Infer ref)
        tc <- readTcRef ref
        return (binds, tc)

tcPat :: (MonadIO m, MonadThrow m) => Pat -> Expected Sigma -> Tc m [(LName, Sigma)]
tcPat WildP _ = return []
tcPat (VarP v) (Infer ref) = do
        ty <- newTyVar
        writeTcRef ref ty
        return [(v, ty)]
tcPat (VarP v) (Check ty) = return [(v, ty)]
tcPat (ConP con ps) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        envs <- zipWithM checkPat ps arg_tys
        _ <- instPatSigma res_ty exp_ty
        return (concat envs)

instPatSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Sigma -> Tc m (Expr -> Expr)
instPatSigma pat_ty (Infer ref) = writeTcRef ref pat_ty >> return id
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

instDataCon :: (MonadIO m, MonadThrow m) => GlbName -> Tc m ([Sigma], Tau)
instDataCon con = do
        con_ty <- lookupVar con
        (_, con_ty') <- instantiate con_ty
        return $ go con_ty' []
    where
        go :: Type -> [Type] -> ([Type], Type)
        go (ArrT arg res) acc = go res (arg : acc)
        go ty acc = (reverse acc, ty)

------------------------------------------
-- tcRho, and its variants
------------------------------------------
checkRho :: (MonadIO m, MonadThrow m) => Expr -> Rho -> Tc m Expr
checkRho expr ty = tcRho expr (Check ty)

inferRho :: (MonadIO m, MonadThrow m) => Expr -> Tc m (Expr, Rho)
inferRho expr = do
        ref <- newTcRef (unreachable "inferRho: empty result")
        expr <- tcRho expr (Infer ref)
        (expr,) <$> readTcRef ref

tcRho :: (MonadIO m, MonadThrow m) => Expr -> Expected Rho -> Tc m Expr
tcRho (VarE v) exp_ty = do
        v_sigma <- lookupVar v
        coercion <- instSigma v_sigma exp_ty
        return $ coercion (VarE v)
tcRho (AppE fun arg) exp_ty = do
        (fun', fun_ty) <- inferRho fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        arg' <- checkSigma arg arg_ty
        coercion <- instSigma res_ty exp_ty
        return $ coercion $ AppE fun' arg'
tcRho (AbsE var Nothing body) (Check exp_ty) = do
        (var_ty, body_ty) <- unifyFun exp_ty
        body' <- extendVarEnv var var_ty (checkRho body body_ty)
        return $ AbsE var (Just var_ty) body'
tcRho (AbsE var Nothing body) (Infer ref) = do
        var_ty <- newTyVar
        (body', body_ty) <- extendVarEnv var var_ty (inferRho body)
        writeTcRef ref (ArrT var_ty body_ty)
        return $ AbsE var (Just var_ty) body'
tcRho (LetE decs body) exp_ty = do
        let exenv = extendVarEnvList [(var, ty) | FuncD var _ ty <- decs]
        (decs', binds) <- execWriterT $
                forM decs $ \(FuncD var var_e ann_ty) -> do
                        (AnnE var_e' _, var_ty) <- Writer.lift $ exenv $ inferSigma (AnnE var_e ann_ty)
                        let d = FuncD var var_e' var_ty
                        tell ([d], [(var, var_ty)])
        body' <- extendVarEnvList binds (tcRho body exp_ty)
        return $ LetE decs' body'
tcRho (CaseE match _ alts) (Check exp_ty) = do
        (match', match_ty) <- inferRho match
        alts' <- forM alts $ \(pat, body) -> do
                args <- checkPat pat match_ty
                body' <- extendVarEnvList args (checkRho body exp_ty)
                return (pat, body')
        return $ CaseE match' (Just match_ty) alts'
tcRho (CaseE match _ alts) (Infer ref) = do
        (match', match_ty) <- inferRho match
        body_tys <- forM alts $ \(pat, body) -> do
                args <- checkPat pat match_ty
                extendVarEnvList args (inferRho body)
        let pairs = filter ((2 ==) . length) $ subsequences body_tys
        forM_ pairs $ \case
                [(_, ty1), (_, ty2)] -> do
                        _ <- subsCheck ty1 ty2
                        subsCheck ty2 ty1
                _ -> unreachable ""
        case body_tys of
                [] -> writeTcRef ref (SumT [])
                (_, ty) : _ -> writeTcRef ref ty
        let alts' = zipWith (\(p, _) (e, _) -> (p, e)) alts body_tys
        return $ CaseE match' (Just match_ty) alts'
tcRho (AnnE body ann_ty) exp_ty = do
        body' <- checkSigma body ann_ty
        _ <- instSigma ann_ty exp_ty --tmp
        return $ AnnE body' ann_ty
tcRho e _ = return e

------------------------------------------
-- inferSigma and checkSigma
------------------------------------------
inferSigma :: (MonadIO m, MonadThrow m) => Expr -> Tc m (Expr, Sigma)
inferSigma e = do
        (expr, exp_ty) <- inferRho e
        env_tys <- getEnvTypes
        env_tvs <- getMetaTvs env_tys
        res_tvs <- getMetaTvs [exp_ty]
        let forall_tvs = res_tvs \\ env_tvs
        if null forall_tvs
                then return (expr, exp_ty)
                else (expr,) <$> quantify forall_tvs exp_ty

checkSigma :: (MonadIO m, MonadThrow m) => Expr -> Sigma -> Tc m Expr
checkSigma expr sigma = do
        (coercion, skol_tvs, rho) <- skolemise sigma
        expr' <- checkRho expr rho
        env_tys <- getEnvTypes
        esc_tvs <- getFreeTyVars (sigma : env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        unless (null bad_tvs) $ lift $ throwUnexpectedErr "Type not polymorphic enough"
        return $ coercion $ if null skol_tvs then expr' else TAbsE (map tyVarLName skol_tvs) expr'

------------------------------------------
--        Subsumption checking          --
------------------------------------------
-- See Technical Appendix p53 for creating coercion terms
subsCheck :: (MonadIO m, MonadThrow m) => Sigma -> Sigma -> Tc m (Expr -> Expr)
subsCheck sigma1 sigma2 = do
        (co1, skol_tvs, rho2) <- skolemise sigma2
        co2 <- subsCheckRho sigma1 rho2
        esc_tvs <- getFreeTyVars [sigma1, sigma2]
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        unless (null bad_tvs) $ lift $ throwUnexpectedErr "Subsumption check failed"
        if null skol_tvs
                then return id
                else return $ \e -> co1 (TAbsE (map tyVarLName skol_tvs) (co2 e))

subsCheckRho :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Tc m (Expr -> Expr)
subsCheckRho sigma1@AllT{} rho2 = do
        (co1, rho1) <- instantiate sigma1
        co2 <- subsCheckRho rho1 rho2
        return $ \e -> (co2 . co1) e
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 a2 r2
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun a1 r1 a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return id

subsCheckFun :: (MonadIO m, MonadThrow m) => Sigma -> Rho -> Sigma -> Rho -> Tc m (Expr -> Expr)
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return
                ( \f ->
                        AbsE
                                (noLoc $ varName "x")
                                (Just a2)
                                (co_res $ AppE f (co_arg (VarE $ newGlbName varName "x")))
                )

instSigma :: (MonadIO m, MonadThrow m) => Sigma -> Expected Rho -> Tc m (Expr -> Expr)
instSigma ty1 (Check ty2) = subsCheckRho ty1 ty2
instSigma ty1 (Infer r) = do
        (coercion, ty1') <- instantiate ty1
        writeTcRef r ty1'
        return coercion
