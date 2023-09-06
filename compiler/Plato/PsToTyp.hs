{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTyp (
        elabExpr,
        elabPat,
        elabType,
        elabDecl,
        elabTopDecls,
        psToTyp,
        psToTypExpr,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List qualified as List
import GHC.Stack
import Plato.Common.Ident

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.PsToTyp.Graph
import Plato.PsToTyp.SynRstrc
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Syntax.Typing.Helper

elabExpr ::
        forall e m.
        (HasCallStack, MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        P.Expr ->
        m (T.Expr 'T.Untyped)
elabExpr scg (P.VarE id) = T.VarE <$> scoping id scg
elabExpr scg (P.AppE fun arg) = T.AppE <$> elabExpr scg `traverse` fun <*> elabExpr scg `traverse` arg
elabExpr scg (P.BinE left op right) = do
        left' <- elabExpr scg `traverse` left
        op' <- scoping op scg
        right' <- elabExpr scg `traverse` right
        return $ T.AppE (sL left' op $ T.AppE (L (getLoc op) (T.VarE op')) left') right'
elabExpr scg (P.LamE pats body) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat scg `traverse`) pats
        body' <- elabExpr (extendScopes pats scg) `traverse` body
        let patlam :: T.LExpr 'T.Untyped -> T.LPat -> m (T.LExpr 'T.Untyped)
            patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id Nothing e
            patlam e p = do
                v <- newVarIdent
                return $ sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        unLoc <$> foldM patlam body' (reverse pats')
elabExpr scg (P.LetE ldecs body) = do
        mapM_ (checkNumArgs . unLoc) ldecs
        let scg' = extendScopes ldecs scg
        bnds <- elabLocDecls scg' ldecs
        body' <- elabExpr scg' `traverse` body
        return $ T.LetE bnds body'
elabExpr scg (P.CaseE match alts) = do
        match' <- elabExpr scg `traverse` match
        alts' <- forM alts $ \(pat, body) -> do
                pat' <- elabPat scg `traverse` pat
                body' <- elabExpr (extendScopes pat scg) `traverse` body
                return (pat', body')

        return $ T.CaseE match' alts'
elabExpr scg (P.AnnE exp ann_ty) = T.AnnE <$> elabExpr scg `traverse` exp <*> elabType scg (unLoc ann_ty)
elabExpr _ P.FactorE{} = unreachable "fixity resolution failed"

elabPat :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => ScopeGraph -> P.Pat -> m T.Pat
elabPat scg (P.ConP con pats) = do
        con' <- scoping con scg
        pats' <- mapM (elabPat scg `traverse`) pats
        return $ T.ConP con' pats'
elabPat _ (P.VarP var) = return $ T.VarP var
elabPat _ P.WildP = return T.WildP
elabPat scg (P.BinP left op right) = do
        left' <- elabPat scg `traverse` left
        op' <- scoping op scg
        right' <- elabPat scg `traverse` right
        return $ T.ConP op' [left', right']
elabPat scg (P.AnnP pat ann_ty) = T.AnnP <$> elabPat scg `traverse` pat <*> elabType scg (unLoc ann_ty)
elabPat _ P.FactorP{} = unreachable "fixity resolution failed"

elabType ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        P.Type ->
        m T.Type
elabType scg (P.VarT var) = do
        var' <- scoping var scg
        return $ T.VarT (T.BoundTv var')
elabType scg (P.ConT con) = T.ConT <$> scoping con scg
elabType scg (P.ArrT arg res) = T.ArrT <$> (elabType scg `traverse` arg) <*> (elabType scg `traverse` res)
elabType scg (P.AllT vars body) = do
        paramNamesUnique vars
        qnts <- mapM (\id -> do kv <- newKnVar; return (T.BoundTv id, kv)) vars
        body' <- elabType (extendScopes vars scg) `traverse` body
        return $ T.AllT qnts body'
elabType scg (P.AppT fun arg) = T.AppT <$> elabType scg `traverse` fun <*> elabType scg `traverse` arg
elabType scg (P.BinT left op right) = do
        left' <- elabType scg `traverse` left
        op' <- scoping op scg
        right' <- elabType scg `traverse` right
        return $ T.AppT (sL left' op $ T.AppT (L (getLoc op) (T.ConT op')) left') right'
elabType _ P.FactorT{} = unreachable "fixity resolution failed"

elabLocDecls ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        [P.LLocDecl] ->
        m [T.Bind 'T.Untyped]
elabLocDecls scg ldecs = do
        ldecs' <- bundleClauses ldecs
        (bnds, spcs) <- execWriterT $ forM ldecs' $ \case
                L _ (P.FunSpecD id ty) -> do
                        id' <- scoping id scg
                        ty' <- elabType scg `traverse` ty
                        tell ([], [(id', ty')])
                L _ (P.FunBindD id clses) -> do
                        clses' <- mapM (elabClause scg) clses
                        tell ([(id, clses')], [])
                L _ P.FixityD{} -> unreachable "deleted by Nicifier"
        forM bnds $ \(id, clses) -> case lookup id spcs of
                Just ty -> return (T.Bind (id, Just ty) clses)
                _ -> return (T.Bind (id, Nothing) clses)

bundleClauses :: MonadThrow m => [P.LLocDecl] -> m [P.LLocDecl]
bundleClauses = classify . partition
    where
        classify :: MonadThrow m => [[P.LLocDecl]] -> m [P.LLocDecl]
        classify [] = return []
        classify (fbnds@(L _ (P.FunBindD id clses) : _) : rest) = do
                let spn = mconcat $ [spi | L spi P.FunBindD{} <- fbnds]
                (L spn (P.FunBindD id clses) :) <$> classify rest
        classify (ldecs : rest) = (ldecs ++) <$> classify rest

        partition :: [P.LLocDecl] -> [[P.LLocDecl]]
        partition =
                List.groupBy $ curry $ \case
                        (L _ (P.FunBindD id1 _), L _ (P.FunBindD id2 _)) -> nameIdent id1 == nameIdent id2
                        _ -> False
elabClause ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        P.Clause ->
        m (T.Clause 'T.Untyped)
elabClause scg (pats, exp) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat scg `traverse`) pats
        exp' <- elabExpr (extendScopes pats scg) `traverse` exp
        return (pats', exp')

elabDecl ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        P.LTopDecl ->
        m (T.TypDefn 'T.Untyped)
elabDecl scg (L _ (P.DataD id params constrs)) = do
        paramNamesUnique params
        dataConUnique $ map fst constrs
        mapM_ (dataConType id) constrs
        qnts <- mapM (\p -> (T.BoundTv p,) <$> newKnVar) params
        constrs' <- forM constrs $
                \(con, ty) -> (con,) <$> elabType (extendScopes (id : params) scg) `traverse` ty
        return $ T.DatDefn id qnts constrs'
elabDecl _ _ = unreachable "fixity resolution failed"

elabTopDecls ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        ScopeGraph ->
        [P.LTopDecl] ->
        m [T.Defn 'T.Untyped]
elabTopDecls scg tdecs = do
        let scg' = extendScopes tdecs scg
        let (tdecs', ldecs) = groupDecl tdecs
        tdefs <- mapM (elabDecl scg') tdecs'
        mapM_ (checkNumArgs . unLoc) ldecs
        binds <- elabLocDecls scg' ldecs
        return [T.TypDefn tdefs, T.ValDefn binds]
    where
        groupDecl :: [P.LTopDecl] -> ([P.LTopDecl], [P.LLocDecl])
        groupDecl decs = execWriter $ forM decs $ \dec -> case dec of
                L _ P.DataD{} -> tell ([dec], [])
                L sp (P.LocalD ld) -> tell ([], [L sp ld])

-----------------------------------------------------------
-- psToTyp
-----------------------------------------------------------

psToTyp :: PlatoMonad m => [P.LTopDecl] -> m (T.Prog 'T.Untyped)
psToTyp tdecs = do
        scg <- initScopeGraph
        catchErrors $ elabTopDecls scg tdecs

psToTypExpr :: PlatoMonad m => P.LExpr -> m (T.LExpr 'T.Untyped)
psToTypExpr exp = do
        scg <- initScopeGraph
        runReaderT (elabExpr scg `traverse` exp) =<< getContext =<< ask