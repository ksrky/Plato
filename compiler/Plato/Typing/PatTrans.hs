{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.PatTrans (transClauses, transCase) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Misc
import Plato.Typing.Zonking

transClauses ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        [Type] ->
        [Clause 'Typed] ->
        m (Expr 'Typed)
transClauses tys clauses = do
        vars <- mapM (\ty -> (,ty) <$> newVarIdent) tys
        exp <- match vars clauses
        return $ foldr (uncurry AbsE') exp vars

transCase ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        Expr 'Typed ->
        m (Expr 'Typed)
transCase (CaseE' exp ty alts) = do
        var <- newVarIdent
        let clauses :: [Clause 'Typed] = map (\(p, e) -> ([p], e)) alts
        altsexp <- match [(var, ty)] clauses
        return $ AppE' (AbsE' var ty altsexp) exp
transCase _ = unreachable "Expected type-checked case expression"

constructors :: forall e m. (MonadReader e m, HasConEnv e, MonadThrow m, MonadIO m) => Type -> m Constrs
constructors ty = getCon [] =<< zonk ty
    where
        getCon :: [Type] -> Type -> m Constrs
        getCon acc (AppT fun arg) = getCon (unLoc arg : acc) (unLoc fun)
        getCon acc (ConT tc) = do
                (params, constrs) <- lookupIdent tc =<< asks getConEnv
                return (map (\(con, ty) -> (con, substTvs params (reverse acc) <$> ty)) constrs)
        getCon _ _ = unreachable "Not a variant type"

subst :: Expr 'Typed -> Expr 'Typed -> Ident -> Expr 'Typed
subst exp replace id = subst' exp
    where
        subst' :: Expr 'Typed -> Expr 'Typed
        subst' (VarE var)
                | var == id = replace
                | otherwise = VarE var
        subst' (AppE' fun arg) = AppE' (subst' fun) (subst' arg)
        subst' (AbsE' var ty body) = AbsE' var ty (subst' body)
        subst' (TAppE exp tyargs) = TAppE (subst' exp) tyargs
        subst' (TAbsE qnts body) = TAbsE qnts (subst' body)
        subst' (LetE' bnds body) =
                LetE' (fmap (\(Bind' idty exp) -> Bind' idty (subst' exp)) bnds) (subst' <$> body)
        subst' (CaseE' test ann_ty alts) =
                CaseE' (subst' test) ann_ty (map (\(pat, exp) -> (pat, subst' exp)) alts)

isVar :: Clause 'Typed -> Bool
isVar (L _ WildP{} : _, _) = True
isVar (L _ VarP{} : _, _) = True
isVar (L _ ConP{} : _, _) = False
isVar (L _ AnnP{} : _, _) = False
isVar (L _ TagP{} : _, _) = unreachable "received TagP"
isVar ([], _) = unreachable "empty pattern row"

isVarorSameCon :: Ident -> Clause 'Typed -> Bool
isVarorSameCon con1 (L _ (ConP con2 _) : _, _) = con1 == con2
isVarorSameCon _ _ = True

match ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (Expr 'Typed)
match [(var, ty)] [] = return (CaseE' (VarE var) ty [])
match _ [] = throwError "Sequence of absurd pattern is not allowed."
match [] (([], exp) : _) = return exp
match [] _ = unreachable "The Number of variables does not equal to the number of patterns"
match (vt : vts) clauses
        | all isVar clauses = matchVar vt vts clauses
        | otherwise = matchCon vt vts clauses

matchVar ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (Expr 'Typed)
matchVar (var, _) rest clauses = do
        let clauses' = (`map` clauses) $ \case
                (L _ WildP : pats, exp) -> (pats, exp)
                (L _ (VarP vp) : pats, exp) -> (pats, subst exp (VarE var) vp)
                (L _ ConP{} : _, _) -> unreachable "ConP"
                (L _ AnnP{} : _, _) -> unreachable "AnnP"
                (L _ TagP{} : _, _) -> unreachable "TagP"
                ([], _) -> unreachable "Number of variables and patterns are not same"
        match rest clauses'

matchCon ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (Expr 'Typed)
matchCon (var, ty) rest clauses = do
        constrs <- constructors ty
        alts <- sequence [matchClause constr rest (choose con clauses) | constr@(con, _) <- constrs]
        return $ CaseE' (VarE var) ty alts

choose :: Ident -> [Clause 'Typed] -> [Clause 'Typed]
choose con clauses = [cls | cls <- clauses, isVarorSameCon con cls]

matchClause ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, [Type]) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (LPat, Expr 'Typed)
matchClause (con, _) _ [] =
        throwError $
                vsep
                        [ "Pattern matching is non-exhaustive."
                        , "Required constructor pattern: " <+> squotes (pretty con)
                        ] -- TODO: error message
matchClause (con, arg_tys) vts clauses = do
        args <- mapM (const newVarIdent) arg_tys
        clauses' <- forM clauses $ \case
                (L _ (ConP _ ps) : ps', e) -> return (ps ++ ps', e)
                (L _ (VarP v) : ps', e) -> do
                        vps <- mapM (const (noLoc . VarP <$> newVarIdent)) arg_tys
                        let con_exp :: Expr 'Typed = foldl AppE' (VarE con) (map VarE args)
                        return (vps ++ ps', subst e con_exp v)
                (L _ WildP : ps', e) -> do
                        vps <- mapM (const (noLoc . VarP <$> newVarIdent)) arg_tys
                        return (vps ++ ps', e)
                (L _ AnnP{} : _, _) -> unreachable " received AnnP"
                (L _ TagP{} : _, _) -> unreachable "received TagP"
                ([], _) -> unreachable "empty patterns"
        let ats = zip args arg_tys
        (noLoc $ TagP con ats,) <$> match (ats ++ vts) clauses'
