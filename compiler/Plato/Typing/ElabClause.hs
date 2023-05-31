{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.ElabClause (elabClauses) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Monad

-- TODO: Each occurance of matching variables' Uniqs in abstractions are identical.
elabClauses ::
        (MonadReader env m, HasUniq env, HasConEnv env, MonadIO m, MonadThrow m) =>
        [Type] ->
        [Clause 'TcDone] ->
        m (LExpr 'TcDone)
elabClauses tys clauses = do
        vars <- mapM (\ty -> (,ty) <$> newVarIdent) tys
        match vars clauses

dataConsof :: (MonadReader env m, HasConEnv env, MonadThrow m) => Type -> m [(Ident, [Type])]
dataConsof ty = do
        let getTycon :: Type -> Ident
            getTycon (AllT _ ty) = getTycon (unLoc ty)
            getTycon (ArrT _ res) = getTycon (unLoc res)
            getTycon (AppT fun _) = getTycon (unLoc fun)
            getTycon (ConT tc) = tc
            getTycon _ = unreachable "Not a variant type"
        lookupIdent (getTycon ty) =<< getConEnv =<< ask

subst :: LExpr 'TcDone -> Ident -> Ident -> LExpr 'TcDone
subst exp id1 id2 = subst' <$> exp
    where
        subst' :: Expr 'TcDone -> Expr 'TcDone
        subst' (VarE var)
                | var == id2 = VarE id1
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
        (MonadReader env m, HasUniq env, HasConEnv env, MonadIO m, MonadThrow m) =>
        [(Ident, Type)] ->
        [Clause 'TcDone] ->
        m (LExpr 'TcDone)
match [(var, ty)] [] = return $ noLoc (AbsEok var ty (CaseEok (noLoc $ VarE var) ty []))
match _ [] = throwError "sequence of absurd type"
match [] (([], exp) : _) = return exp -- note: clauses should be singleton if not redundant
match [] _ = unreachable "Number of variables and patterns are not same"
match (u : us) clauses
        | all isVar clauses = matchVar u us clauses
        | otherwise = matchCon u us clauses

matchVar ::
        (MonadReader env m, HasUniq env, HasConEnv env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'TcDone] ->
        m (LExpr 'TcDone)
matchVar (var, ty) rest clauses = do
        let clauses' = (`map` clauses) $ \case
                (L _ WildP : pats, exp) -> (pats, L (getLoc exp) $ AbsEok var ty (unLoc exp))
                (L _ (VarP varp) : pats, exp) ->
                        (pats, L (getLoc exp) $ AbsEok var ty (unLoc $ subst exp var varp))
                (L _ ConP{} : _, _) -> unreachable "ConP"
                ([], _) -> unreachable "Number of variables and patterns are not same"
        match rest clauses'

-- [(pats, subst exp var varp) | (L _ (VarP varp) : pats, exp) <- clauses]

matchCon ::
        (MonadReader env m, HasUniq env, HasConEnv env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause 'TcDone] ->
        m (LExpr 'TcDone)
matchCon (var, ty) rest clauses = do
        constrs <- dataConsof ty
        alts <- sequence [matchClause constr rest (choose con clauses) | constr@(con, _) <- constrs]
        return $ noLoc $ AbsEok var ty (CaseEok (noLoc $ VarE var) ty alts)

matchClause ::
        (MonadReader env m, HasUniq env, HasConEnv env, MonadIO m, MonadThrow m) =>
        (Ident, [Type]) ->
        [(Ident, Type)] ->
        [Clause 'TcDone] ->
        m (LPat, LExpr 'TcDone)
matchClause (con, arg_tys) vars clauses = do
        params <- mapM (const newVarIdent) arg_tys
        let pat = noLoc $ ConP con (map (noLoc . VarP) params)
            vars' = zip params arg_tys ++ vars
            clauses' = [(ps ++ ps', e) | (L _ (ConP _ ps') : ps, e) <- clauses]
        (pat,) <$> match vars' clauses'

choose :: Ident -> [Clause 'TcDone] -> [Clause 'TcDone]
choose con clauses = [cls | cls <- clauses, isVarorSameCon con cls]