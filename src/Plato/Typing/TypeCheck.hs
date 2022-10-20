{-# LANGUAGE TupleSections #-}

module Plato.Typing.TypeCheck where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.Monad
import Plato.Typing.Types

import Control.Monad
import Control.Monad.Writer
import Data.IORef
import Data.List

typecheck :: Located Expr -> Located Type -> Typ Sigma
typecheck e ty = do
        (e', ty') <- inferSigma (AnnE e ty)
        zonkType ty'

data Expected a = Infer (IORef a) | Check a

------------------------------------------
-- tcPat, and its variants
------------------------------------------
-- tmp: no translation for patterns
-- because pattern match is implemented by classifying data consructor's tag.
-- If type application is inserted in ConP, GADT will be available.
checkPat :: Pat -> Rho -> Typ [(Located Name, Sigma)]
checkPat pat ty = tcPat pat (Check ty)

inferPat :: Pat -> Typ ([(Located Name, Sigma)], Sigma)
inferPat pat = do
        ref <- newTypRef (error "inferRho: empty result")
        binds <- tcPat pat (Infer ref)
        tc <- readTypRef ref
        return (binds, tc)

tcPat :: Pat -> Expected Sigma -> Typ [(Located Name, Sigma)]
tcPat WildP _ = return []
tcPat (VarP v) (Infer ref) = do
        ty <- newTyVar
        writeTypRef ref ty
        return [(v, ty)]
tcPat (VarP v) (Check ty) = return [(v, ty)]
tcPat (ConP con ps) exp_ty = do
        (arg_tys, res_ty) <- instDataCon con
        envs <- mapM check_arg (ps `zip` arg_tys)
        instPatSigma res_ty exp_ty
        return (concat envs)
    where
        check_arg (p, ty) = checkPat p ty

instPatSigma :: Sigma -> Expected Sigma -> Typ (Expr -> Expr)
instPatSigma pat_ty (Infer ref) = writeTypRef ref pat_ty >> return id
instPatSigma pat_ty (Check exp_ty) = subsCheck exp_ty pat_ty

instDataCon :: Located Name -> Typ ([Sigma], Tau) --tmp: data constructor be in TypEnv
instDataCon con = do
        con_ty <- lookupVar con
        con_ty' <- instantiate con_ty
        return $ go con_ty' []
    where
        go :: Type -> [Type] -> ([Type], Type)
        go (ArrT arg res) acc = go (unLoc res) (unLoc arg : acc)
        go ty acc = (reverse acc, ty)

------------------------------------------
-- TypRho, and its variants
------------------------------------------
checkRho :: Expr -> Rho -> Typ Expr
checkRho expr ty = typRho expr (Check ty)

inferRho :: Expr -> Typ (Expr, Rho)
inferRho expr = do
        ref <- newTypRef (error "inferRho: empty result")
        expr <- typRho expr (Infer ref)
        (expr,) <$> readTypRef ref

typRho :: Expr -> Expected Rho -> Typ Expr
typRho (VarE v) exp_ty = do
        v_sigma <- lookupVar v
        coercion <- instSigma v_sigma exp_ty
        return $ coercion (VarE v)
typRho (AppE fun arg) exp_ty = do
        (fun', fun_ty) <- inferRho $ unLoc fun
        (arg_ty, res_ty) <- unifyFun fun_ty
        checkSigma (unLoc arg) arg_ty
        coercion <- instSigma res_ty exp_ty
        return $ coercion $ AppE (L (getSpan fun) fun') arg
typRho (AbsE var Nothing body) (Check exp_ty) = do
        (var_ty, body_ty) <- unifyFun exp_ty
        body' <- extendVarEnv (unLoc var) var_ty (checkRho (unLoc body) body_ty)
        return $ AbsE var (Just var_ty) (L (getSpan body) body')
typRho (AbsE var Nothing body) (Infer ref) = do
        var_ty <- newTyVar
        (body', body_ty) <- extendVarEnv (unLoc var) var_ty (inferRho $ unLoc body)
        writeTypRef ref (ArrT (noLoc var_ty) (noLoc body_ty))
        return $ AbsE var (Just var_ty) (L (getSpan body) body')
typRho (LetE decs body) exp_ty = do
        (decs', binds) <- execWriterT $
                forM decs $ \(FD var var_e ann_ty) -> do
                        (var_e', var_ty) <- Control.Monad.Writer.lift $ inferSigma (AnnE var_e ann_ty)
                        let d = FD var (L (getSpan var_e) var_e') (L (getSpan ann_ty) var_ty)
                        tell ([d], [(unLoc var, var_ty)])
        body' <- extendVarEnvList binds (typRho (unLoc body) exp_ty)
        return $ LetE decs' (L (getSpan body) body')
typRho (ProjE exp lab) exp_ty = undefined
typRho (RecordE fields) exp_ty = undefined
typRho (CaseE match _ alts) (Check exp_ty) = do
        (match', match_ty) <- inferRho $ unLoc match
        alts' <- forM alts $ \(pat, body) -> do
                checkPat (unLoc pat) match_ty
                body' <- checkRho (unLoc body) exp_ty
                return (pat, L (getSpan body) body')
        return $ CaseE (L (getSpan match) match') (Just match_ty) alts'
typRho (CaseE match _ alts) (Infer ref) = do
        (match', match_ty) <- inferRho $ unLoc match
        body_tys <- forM alts $ \(pat, body) -> do
                checkPat (unLoc pat) match_ty
                inferRho (unLoc body)
        let pairs = filter ((2 ==) . length) $ subsequences body_tys
        forM_ pairs $ \[(e1, ty1), (e2, ty2)] -> do
                subsCheck ty1 ty2
                subsCheck ty2 ty1
        return $ CaseE (L (getSpan match) match') (Just match_ty) alts
typRho TagE{} exp_ty = undefined
typRho (AnnE body ann_ty) exp_ty = do
        checkSigma (unLoc body) (unLoc ann_ty)
        coercion <- instSigma (unLoc ann_ty) exp_ty
        return $ AnnE (coercion <$> body) ann_ty
typRho _ _ = unreachable "TAbsExpr, TAppExpr"

------------------------------------------
-- inferSigma and checkSigma
------------------------------------------
inferSigma :: Expr -> Typ (Expr, Sigma)
inferSigma e = do
        (expr, exp_ty) <- inferRho e
        env_tys <- getEnvTypes
        env_tvs <- getMetaTyVars env_tys
        res_tvs <- getMetaTyVars [exp_ty]
        let forall_tvs = res_tvs \\ env_tvs
        if null forall_tvs
                then return (expr, exp_ty)
                else (expr,) <$> quantify forall_tvs exp_ty

checkSigma :: Expr -> Sigma -> Typ ()
checkSigma expr sigma = do
        (skol_tvs, rho, coercion) <- skolemise sigma
        checkRho expr rho
        env_tys <- getEnvTypes
        esc_tvs <- getFreeTyVars (sigma : env_tys)
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "Type not polymorphic enough" --tmp

------------------------------------------
--        Subsumption checking          --
------------------------------------------
-- See Technical Appendix p53 for creating coercion terms
subsCheck :: Sigma -> Sigma -> Typ (Expr -> Expr)
subsCheck sigma1 sigma2 = do
        (skol_tvs, rho2, co1) <- skolemise sigma2
        co2 <- subsCheckRho sigma1 rho2
        esc_tvs <- getFreeTyVars [sigma1, sigma2]
        let bad_tvs = filter (`elem` esc_tvs) skol_tvs
        check (null bad_tvs) NoSpan "error" --tmp
        if null skol_tvs
                then return id
                else return $ \x -> co1 (TAbsE (map tyVarName skol_tvs) (noLoc $ co2 x))

subsCheckRho :: Sigma -> Rho -> Typ (Expr -> Expr)
subsCheckRho sigma1@(AllT tvs _) rho2 = do
        rho1 <- instantiate sigma1
        coercion <- subsCheckRho rho1 rho2
        if null tvs
                then return id
                else return $ \x -> coercion $ TAppE (noLoc x) (map VarT tvs)
subsCheckRho rho1 (ArrT a2 r2) = do
        (a1, r1) <- unifyFun rho1
        subsCheckFun a1 r1 (unLoc a2) (unLoc r2)
subsCheckRho (ArrT a1 r1) rho2 = do
        (a2, r2) <- unifyFun rho2
        subsCheckFun (unLoc a1) (unLoc r1) a2 r2
subsCheckRho tau1 tau2 = do
        unify tau1 tau2
        return id

subsCheckFun :: Sigma -> Rho -> Sigma -> Rho -> Typ (Expr -> Expr)
subsCheckFun a1 r1 a2 r2 = do
        co_arg <- subsCheck a2 a1
        co_res <- subsCheckRho r1 r2
        return (\f -> AbsE (noLoc $ str2varName "x") (Just a2) (noLoc $ co_res $ AppE (noLoc f) (noLoc $ co_arg (VarE $ noLoc $ str2varName "x"))))

instSigma :: Sigma -> Expected Rho -> Typ (Expr -> Expr)
instSigma t1 (Check t2) = subsCheckRho t1 t2
instSigma t1 (Infer r) = do
        t1' <- instantiate t1
        writeTypRef r t1'
        return id -- tmp: ?
