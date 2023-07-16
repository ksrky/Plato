{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.ElabClause (elabClauses, elabCase) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Logger
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Utils
import System.Log.Logger

elabClauses ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        [Type] ->
        [Clause 'Typed] ->
        m (LExpr 'Typed)
elabClauses tys clauses = do
        vars <- mapM (\ty -> (,ty) <$> newVarIdent) tys
        exp <- match vars clauses
        return $ L (getLoc exp) $ foldr (uncurry AbsEok) (unLoc exp) vars

elabCase ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        Expr 'Typed ->
        m (Expr 'Typed)
elabCase (CaseEok exp ty alts) = do
        var <- newVarIdent
        let clauses :: [Clause 'Typed] = map (\(p, e) -> ([p], e)) alts
        altsexp <- match [(var, ty)] clauses
        return $ AppE (AbsEok var ty <$> altsexp) exp
elabCase _ = unreachable "Expected case expression"

dataConsof :: (MonadReader env m, HasConEnv env, MonadThrow m) => Type -> m Constrs
dataConsof ty = do
        let getTycon :: Type -> Ident
            getTycon (AllT _ ty) = getTycon (unLoc ty)
            getTycon (ArrT _ res) = getTycon (unLoc res)
            getTycon (AppT fun _) = getTycon (unLoc fun)
            getTycon (ConT tc) = tc
            getTycon _ = unreachable "Not a variant type"
        lookupIdent (getTycon ty) =<< asks getConEnv

subst :: LExpr 'Typed -> Expr 'Typed -> Ident -> LExpr 'Typed
subst exp replace id = subst' <$> exp
    where
        subst' :: Expr 'Typed -> Expr 'Typed
        subst' (VarE var)
                | var == id = replace
                | otherwise = VarE var
        subst' (AppE fun arg) = AppE (subst' <$> fun) (subst' <$> arg)
        subst' (AbsEok var ty body) = AbsEok var ty (subst' body)
        subst' (TAppE exp tyargs) = TAppE (subst' exp) tyargs
        subst' (TAbsE qnts body) = TAbsE qnts (subst' body)
        subst' (LetEok bnds sigs body) =
                LetEok (map (\(id, exp) -> (id, subst' <$> exp)) bnds) sigs (subst' <$> body)
        subst' (CaseEok match ty alts) =
                CaseEok (subst' <$> match) ty (map (\(id, exp) -> (id, subst' <$> exp)) alts)

isVar :: Clause a -> Bool
isVar (L _ WildP{} : _, _) = True
isVar (L _ VarP{} : _, _) = True
isVar (L _ ConP{} : _, _) = False
isVar ([], _) = unreachable "ElabClause.isVar"

isVarorSameCon :: Ident -> Clause a -> Bool
isVarorSameCon con1 (L _ (ConP con2 _) : _, _) = con1 == con2
isVarorSameCon _ _ = True

match ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (LExpr 'Typed)
match [(var, ty)] [] = return $ noLoc (CaseEok (noLoc $ VarE var) ty [])
match _ [] = throwError "sequence of absurd type"
match [] (([], exp) : _) = return exp -- note: clauses should be singleton if not redundant
match [] _ = unreachable "Number of variables and patterns are not same"
match (u : us) clauses
        | all isVar clauses = matchVar u us clauses
        | otherwise = matchCon u us clauses

matchVar ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (LExpr 'Typed)
matchVar (var, _) rest clauses = do
        when (length clauses > 1) $ liftIO $ warningM platoLog "Pattern matching is redundant."
        let clauses' = (`map` clauses) $ \case
                (L _ WildP : pats, exp) -> (pats, exp)
                (L _ (VarP varp) : pats, exp) ->
                        (pats, subst exp (VarE var) varp)
                (L _ ConP{} : _, _) -> unreachable "ConP"
                ([], _) -> unreachable "Number of variables and patterns are not same"
        match rest clauses'

matchCon ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (LExpr 'Typed)
matchCon (var, ty) rest clauses = do
        constrs <- dataConsof ty
        alts <- sequence [matchClause constr rest (choose con clauses) | constr@(con, _) <- constrs]
        return $ noLoc $ CaseEok (noLoc $ VarE var) ty alts

matchClause ::
        (MonadReader env m, HasConEnv env, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, [Type]) ->
        [(Ident, Type)] ->
        [Clause 'Typed] ->
        m (LPat, LExpr 'Typed)
matchClause (con, _) _ [] =
        throwError $
                hsep ["Pattern matching is non-exhaustive. Required", squotes $ pretty con]
matchClause (con, arg_tys) vars clauses = do
        params <- mapM (const newVarIdent) arg_tys
        let pat = noLoc $ ConP con (map (noLoc . VarP) params)
            vars' = zip params arg_tys ++ vars
        clauses' <- forM clauses $ \case
                (L _ (ConP _ ps) : ps', e) -> return (ps ++ ps', e)
                (L _ (VarP v) : ps', e) -> do
                        vps <- mapM (const (noLoc . VarP <$> newVarIdent)) arg_tys
                        let con_exp :: Expr 'Typed =
                                foldl (AppE . noLoc) (VarE con) (map (noLoc . VarE) params)
                        return (vps ++ ps', subst e con_exp v)
                (L _ WildP : ps', e) -> do
                        vps <- mapM (const (noLoc . VarP <$> newVarIdent)) arg_tys
                        return (vps ++ ps', e)
                ([], _) -> unreachable "empty patterns"
        (pat,) <$> match vars' clauses'

choose :: Ident -> [Clause 'Typed] -> [Clause 'Typed]
choose con clauses = [cls | cls <- clauses, isVarorSameCon con cls]
