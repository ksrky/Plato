{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTyp (
        elabExpr,
        elabPat,
        elabType,
        elabDecls,
        elabTopDecls,
        psToTyp,
        psToTypExpr,
) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.PsToTyp.Scoping
import Plato.PsToTyp.SynRstrc
import Plato.PsToTyp.Utils
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Syntax.Typing.Helper

elabExpr ::
        forall e m.
        (HasCallStack, MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        P.Expr ->
        m (T.Expr 'T.Untyped)
elabExpr (P.VarE id) = T.VarE <$> scoping id
elabExpr (P.AppE fun arg) = T.AppE <$> elabExpr `traverse` fun <*> elabExpr `traverse` arg
elabExpr (P.BinE left op right) = do
        left' <- elabExpr `traverse` left
        op' <- scoping op
        right' <- elabExpr `traverse` right
        return $ T.AppE (sL left' op $ T.AppE (L (getLoc op) (T.VarE op')) left') right'
elabExpr (P.LamE pats body) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        env <- extendScopeFromSeq pats
        body' <- local (const env) $ elabExpr `traverse` body
        let patlam :: T.LExpr 'T.Untyped -> T.LPat -> m (T.LExpr 'T.Untyped)
            patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id Nothing e
            patlam e p = do
                v <- newVarIdent
                return $ sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        unLoc <$> foldM patlam body' (reverse pats')
elabExpr (P.LetE ldecs body) = do
        ldecs' <- bundleClauses ldecs
        env <- extendScopeFromSeq ldecs'
        local (const env) $ do
                bnds <- elabLocDecls ldecs'
                body' <- elabExpr `traverse` body
                return $ T.LetE bnds body'
elabExpr (P.CaseE match alts) = do
        match' <- elabExpr `traverse` match
        alts' <- forM alts $ \(pat, body) -> do
                pat' <- elabPat `traverse` pat
                body' <- local (extendListScope $ getDomain pat) (elabExpr `traverse` body)
                return (pat', body')

        return $ T.CaseE match' alts'
elabExpr (P.AnnE exp ann_ty) = T.AnnE <$> elabExpr `traverse` exp <*> elabType (unLoc ann_ty)
elabExpr P.FactorE{} = unreachable "fixity resolution failed"

elabPat :: (MonadReader e m, HasScope e, HasUniq e, MonadIO m, MonadThrow m) => P.Pat -> m T.Pat
elabPat (P.ConP con pats) = do
        con' <- scoping con
        pats' <- mapM (elabPat `traverse`) pats
        return $ T.ConP con' pats'
elabPat (P.VarP var) = return $ T.VarP var
elabPat P.WildP = return T.WildP
elabPat (P.BinP left op right) = do
        left' <- elabPat `traverse` left
        op' <- scoping op
        right' <- elabPat `traverse` right
        return $ T.ConP op' [left', right']
elabPat (P.AnnP pat ann_ty) = T.AnnP <$> elabPat `traverse` pat <*> elabType (unLoc ann_ty)
elabPat P.FactorP{} = unreachable "fixity resolution failed"

elabType ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        P.Type ->
        m T.Type
elabType (P.VarT var) = do
        var' <- scoping var
        return $ T.VarT (T.BoundTv var')
elabType (P.ConT con) = T.ConT <$> scoping con
elabType (P.ArrT arg res) = T.ArrT <$> (elabType `traverse` arg) <*> (elabType `traverse` res)
elabType (P.AllT vars body) = do
        paramNamesUnique vars
        qnts <- mapM (\id -> do kv <- newKnVar; return (T.BoundTv id, kv)) vars
        body' <- local (extendListScope vars) $ elabType `traverse` body
        return $ T.AllT qnts body'
elabType (P.AppT fun arg) = T.AppT <$> elabType `traverse` fun <*> elabType `traverse` arg
elabType (P.BinT left op right) = do
        left' <- elabType `traverse` left
        op' <- scoping op
        right' <- elabType `traverse` right
        return $ T.AppT (sL left' op $ T.AppT (L (getLoc op) (T.ConT op')) left') right'
elabType P.FactorT{} = unreachable "fixity resolution failed"

elabLocDecls ::
        (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) =>
        [P.LLocDecl] ->
        m [T.Bind 'T.Untyped]
elabLocDecls fdecs = do
        (bnds, spcs) <- execWriterT $ forM fdecs $ \case
                L _ (P.FunSpecD id ty) -> do
                        id' <- scoping id
                        ty' <- elabType `traverse` ty
                        tell ([], [(id', ty')])
                L _ (P.FunBindD id clses) -> do
                        clses' <- mapM elabClause clses
                        tell ([(id, clses')], [])
                L _ P.FixityD{} -> unreachable "deleted by Nicifier"
        forM bnds $ \(id, clses) -> case lookup id spcs of
                Just ty -> return (T.Bind (id, Just ty) clses)
                _ -> return (T.Bind (id, Nothing) clses)

elabClause ::
        (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) =>
        P.Clause ->
        m (T.Clause 'T.Untyped)
elabClause (pats, exp) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        env <- extendScopeFromSeq pats
        exp' <- local (const env) $ elabExpr `traverse` exp
        return (pats', exp')

elabDecls ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        [P.LTopDecl] ->
        WriterT [T.Defn 'T.Untyped] m e
elabDecls [] = ask
elabDecls (L _ (P.DataD id params constrs) : rest) = do
        paramNamesUnique params
        dataConUnique $ map fst constrs
        mapM_ (dataConType id) constrs
        qnts <- mapM (\p -> (T.BoundTv p,) <$> newKnVar) params
        constrs' <-
                local (extendListScope $ id : params) $
                        mapM (\(con, ty) -> (con,) <$> elabType `traverse` ty) constrs
        tell [T.TypDefn [T.DatDefn id qnts constrs']]
        local (extendListScope (id : map fst constrs)) $ elabDecls rest
elabDecls ldecs = do
        -- Note: Nicifier ordered from data decls to local decls
        ldecs' <- bundleClauses (map (P.unLocalD <$>) ldecs)
        env <- extendScopeFromSeq ldecs
        local (const env) $ do
                binds <- elabLocDecls ldecs'
                tell [T.ValDefn binds]
                ask

elabTopDecls ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        [P.LTopDecl] ->
        m ([T.Defn 'T.Untyped], e)
elabTopDecls tdecs = do
        (env, defs) <- runWriterT (elabDecls tdecs)
        return (defs, env)

-----------------------------------------------------------
-- psToTyp
-----------------------------------------------------------

psToTyp :: PlatoMonad m => [P.LTopDecl] -> m (T.Prog 'T.Untyped)
psToTyp tdecs = catchErrors $ updateContext (elabTopDecls tdecs)

psToTypExpr :: PlatoMonad m => P.LExpr -> m (T.LExpr 'T.Untyped)
psToTypExpr exp = runReaderT (elabExpr `traverse` exp) =<< getContext =<< ask