{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Typing.PatTrans (transClauses, transCase) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper
import Plato.Typing.Env
import Plato.Typing.Misc
import Plato.Typing.Zonking

transClauses ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        [Type] ->
        Clauses 'Typed ->
        m (Expr 'Typed)
transClauses tys clauses = do
        vars <- zipWithM (\ty i -> (,ty) <$> labelVarId ("sv" ++ show i)) tys [1 :: Integer ..]
        exp <- match vars clauses
        return $ foldr (uncurry AbsE') exp vars

transCase ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        Expr 'Typed ->
        m (Expr 'Typed)
transCase (CaseE' exp ty alts) = do
        var <- labelVarId "cv"
        let clauses :: Clauses 'Typed = map (\(p, e) -> ([p], e)) alts
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
                mapM (\(con, tys) -> (con,) <$> mapM (substTvs params (reverse acc)) tys) constrs
        getCon _ _ = unreachable "Not a variant type"

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
        Clauses 'Typed ->
        m (Expr 'Typed)
match [(var, ty)] [] = return (CaseE' (VarE var) ty [])
match _ [] = throwError "Sequence of absurd pattern is not allowed."
match [] (([], exp) : _) = return exp
match [] _ = unreachable "The Number of variables does not equal to the number of patterns"
match (vt : vts) clauses
        | all isVar clauses = matchVar vt vts clauses
        | otherwise = matchCon vt vts clauses

matchVar ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        Clauses 'Typed ->
        m (Expr 'Typed)
matchVar (var, _) rest clauses = do
        let clauses' = (`map` clauses) $ \case
                (L _ WildP : pats, exp) -> (pats, exp)
                (L _ (VarP vp) : pats, exp) -> (pats, substExpr vp (VarE var) exp)
                (L _ ConP{} : _, _) -> unreachable "ConP"
                (L _ AnnP{} : _, _) -> unreachable "AnnP"
                (L _ TagP{} : _, _) -> unreachable "TagP"
                ([], _) -> unreachable "Number of variables and patterns are not same"
        match rest clauses'

matchCon ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        (Ident, Type) ->
        [(Ident, Type)] ->
        Clauses 'Typed ->
        m (Expr 'Typed)
matchCon (var, ty) rest clauses = do
        constrs <- constructors ty
        alts <- sequence [matchClause constr rest (choose con clauses) | constr@(con, _) <- constrs]
        return $ CaseE' (VarE var) ty alts

choose :: Ident -> Clauses 'Typed -> Clauses 'Typed
choose con clauses = [cls | cls <- clauses, isVarorSameCon con cls]

matchClause ::
        (MonadReader e m, HasConEnv e, HasUniq e, MonadIO m, MonadThrow m) =>
        (Ident, [Type]) ->
        [(Ident, Type)] ->
        Clauses 'Typed ->
        m (LPat, Expr 'Typed)
matchClause (con, _) _ [] =
        throwError
                $ vsep
                        [ "Pattern matching is non-exhaustive."
                        , "Required constructor pattern: " <+> squotes (pretty con)
                        ] -- TODO: error message
matchClause (con, arg_tys) vts clauses = do
        args <- mapM (\i -> labelVarId ("pv" ++ show i)) [1 .. length arg_tys]
        clauses' <- forM clauses $ \case
                (L _ (ConP _ ps) : ps', e) -> return (ps ++ ps', e)
                (L _ (VarP v) : ps', e) -> do
                        let con_exp = foldl AppE' (VarE con) (map VarE args)
                            dummy_pats = map (const $ noLoc WildP) arg_tys
                        return (dummy_pats ++ ps', substExpr v con_exp e)
                (L _ WildP : ps', e) -> do
                        let dummy_pats = map (const $ noLoc WildP) arg_tys
                        return (dummy_pats ++ ps', e)
                (L _ AnnP{} : _, _) -> unreachable " received AnnP"
                (L _ TagP{} : _, _) -> unreachable "received TagP"
                ([], e) -> return ([], e)
        let ats = zip args arg_tys
        (noLoc $ TagP con ats,) <$> match (ats ++ vts) clauses'
