module Plato.Typing.TypeCheck where

import Plato.Syntax.Typing
import Plato.Typing.Monad
import Plato.Typing.Types

import Data.IORef
import Data.List ((\\))
import Plato.Common.Error
import Plato.Common.SrcLoc

typecheck :: Expr -> Typ Sigma
typecheck e = do
        ty <- inferSigma e
        zonkType ty

data Expected a = Infer (IORef a) | Check a

------------------------------------------
-- TypRho, and its variants
------------------------------------------
checkRho :: Expr -> Rho -> Typ ()
checkRho expr ty = typRho expr (Check ty)

inferRho :: Expr -> Typ Rho
inferRho expr = do
        ref <- newTypRef (error "inferRho: empty result")
        typRho expr (Infer ref)
        readTypRef ref

typRho :: Expr -> Expected Rho -> Typ ()
typRho (VarExpr v) exp_ty = do
        v_sigma <- lookupVar v
        instSigma v_sigma exp_ty
typRho (AppExpr fun arg) exp_ty = do
        fun_ty <- inferRho $ unLoc fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        checkSigma (unLoc arg) arg_ty
        instSigma res_ty exp_ty
typRho (AbsExpr var Nothing body) (Check exp_ty) = do
        (var_ty, body_ty) <- unifyFun exp_ty
        extendVarEnv (unLoc var) var_ty (checkRho (unLoc body) body_ty)
typRho (AbsExpr var Nothing (L sp body)) (Infer ref) = do
        var_ty <- newTyVar
        body_ty <- extendVarEnv (unLoc var) var_ty (inferRho body)
        writeTypRef ref (ArrType (noLoc var_ty) (noLoc body_ty))
typRho (LetExpr (FD var bind ann_ty) body) exp_ty = do
        var_ty <- inferSigma (AnnExpr bind ann_ty)
        extendVarEnv (unLoc var) var_ty (typRho (unLoc body) exp_ty)
typRho (ProjExpr exp lab) exp_ty = undefined
typRho (RecordExpr fields) exp_ty = undefined
typRho CaseExpr{} exp_ty = undefined
typRho TagExpr{} exp_ty = undefined
typRho (AnnExpr body ann_ty) exp_ty = do
        checkSigma (unLoc body) (unLoc ann_ty)
        instSigma (unLoc ann_ty) exp_ty
typRho _ _ = unreachable "TAbsExpr, TAppExpr"

------------------------------------------
-- inferSigma and checkSigma
------------------------------------------
inferSigma :: Expr -> Typ Sigma
inferSigma e = do
        exp_ty <- inferRho e
        env_tys <- getEnvTypes
        env_tvs <- getMetaTyVars env_tys
        res_tvs <- getMetaTyVars [exp_ty]
        let forall_tvs = res_tvs \\ env_tvs
        if null forall_tvs
                then return exp_ty
                else quantify forall_tvs exp_ty

checkSigma :: Expr -> Sigma -> Typ ()
checkSigma expr sigma = do
        (skol_tvs, rho) <- skolemise sigma
        checkRho expr rho
        env_tys <- getEnvTypes
        esc_tvs <- getFreeTyVars (sigma : env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "Type not polymorphic enough" --tmp

------------------------------------------
--        Subsumption checking          --
------------------------------------------
subsCheck :: Sigma -> Sigma -> Typ ()
subsCheck sigma1 sigma2 = do
        (skol_tvs, rho2) <- skolemise sigma2
        subsCheckRho sigma1 rho2
        esc_tvs <- getFreeTyVars [sigma1, sigma2]
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "error" --tmp

subsCheckRho :: Sigma -> Rho -> Typ ()
subsCheckRho sigma1@(AllType _ _) rho2 = do
        rho1 <- instantiate sigma1
        subsCheckRho rho1 rho2
subsCheckRho rho1 (ArrType a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrType a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = unify tau1 tau2

subsCheckFun :: Sigma -> Rho -> Sigma -> Rho -> Typ ()
subsCheckFun a1 r1 a2 r2 = do
        subsCheck a2 a1
        subsCheckRho r1 r2

instSigma :: Sigma -> Expected Rho -> Typ ()
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
        t1' <- instantiate t1
        writeTypRef r t1'
