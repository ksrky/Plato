{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.Typing.ElabClause (elabClauses) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Monad

elabClauses :: (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) => [Type] -> [Clause] -> m Clause
elabClauses tys clauses = do
        vars <- mapM (\ty -> (,ty) <$> newVarIdent) tys
        exp <- match vars clauses
        return ([], exp)

constrArgTys :: (MonadReader env m, HasUniq env) => Ident -> m [Type]
constrArgTys = undefined

constrsOf :: (MonadReader env m, HasUniq env) => Type -> m [Ident]
constrsOf = undefined

subst :: LExpr -> Ident -> Ident -> LExpr
subst = undefined

isVar :: Clause -> Bool
isVar (L _ WildP{} : _, _) = True
isVar (L _ VarP{} : _, _) = True
isVar (L _ ConP{} : _, _) = False
isVar ([], _) = unreachable ""

isVarorSameCon :: Ident -> Clause -> Bool
isVarorSameCon con1 (L _ (ConP con2 _) : _, _) = con1 == con2
isVarorSameCon _ _ = True

match :: (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) => [(Ident, Type)] -> [Clause] -> m LExpr
match [var] [] = return $ noLoc (CaseE (noLoc $ VarE $ fst var) [])
match _ [] = throwError "absurd type"
match [] (([], exp) : _) = return exp -- note: clauses should be singleton if not redundant
match [] _ = unreachable "Number of variables and patterns are not same"
match (u : us) clauses
        | all isVar clauses = matchVar u us clauses
        | otherwise = matchCon u us clauses

matchVar ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause] ->
        m LExpr
matchVar (var, _) rest clauses = do
        let clauses' = (`map` clauses) $ \case
                (L _ WildP : pats, exp) -> (pats, exp)
                (L _ (VarP varp) : pats, exp) -> (pats, subst exp var varp)
                (L _ ConP{} : _, _) -> unreachable "ConP"
                ([], _) -> unreachable "Number of variables and patterns are not same"
        match rest clauses'

-- [(pats, subst exp var varp) | (L _ (VarP varp) : pats, exp) <- clauses]

matchCon ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        [Clause] ->
        m LExpr
matchCon (var, ty) rest clauses = do
        constrs <- constrsOf ty
        alts <- sequence [matchClause con rest (choose con clauses) | con <- constrs]
        return $ noLoc $ CaseE (noLoc $ VarE var) alts

matchClause ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        Ident ->
        [(Ident, Type)] ->
        [Clause] ->
        m (LPat, LExpr)
matchClause con vars clauses = do
        arg_tys <- constrArgTys con
        params <- mapM (const newVarIdent) arg_tys
        let pat = noLoc $ ConP con (map (noLoc . VarP) params)
            vars' = zip params arg_tys ++ vars
            clauses' = [(ps ++ ps', e) | (L _ (ConP _ ps') : ps, e) <- clauses]
        (pat,) <$> match vars' clauses'

choose :: Ident -> [Clause] -> [Clause]
choose con clauses = [cls | cls <- clauses, isVarorSameCon con cls]

{-
canonCase :: LExpr -> [Clause] -> LExpr
canonCase test alts = match 0 [test] (map (\(p, e) -> ([p], e)) alts) undefined-}