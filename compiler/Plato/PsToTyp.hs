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
import Plato.PsToTyp.Scoping
import Plato.PsToTyp.SynRstrc
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Syntax.Typing.Helper

elabExpr ::
        forall e m.
        (HasCallStack, MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Scope ->
        P.Expr ->
        m (T.Expr 'T.Untyped)
elabExpr sc (P.VarE id) = T.VarE <$> scoping id sc
elabExpr sc (P.AppE fun arg) = T.AppE <$> elabExpr sc `traverse` fun <*> elabExpr sc `traverse` arg
elabExpr sc (P.BinE left op right) = do
        left' <- elabExpr sc `traverse` left
        op' <- scoping op sc
        right' <- elabExpr sc `traverse` right
        return $ T.AppE (sL left' op $ T.AppE (L (getLoc op) (T.VarE op')) left') right'
elabExpr sc (P.LamE pats body) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat sc `traverse`) pats
        body' <- elabExpr (extendScope pats sc) `traverse` body
        let patlam :: T.LExpr 'T.Untyped -> T.LPat -> m (T.LExpr 'T.Untyped)
            patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id Nothing e
            patlam e p = do
                v <- newVarIdent
                return $ sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        unLoc <$> foldM patlam body' (reverse pats')
elabExpr sc (P.LetE ldecs body) = do
        mapM_ (checkNumArgs . unLoc) ldecs
        ldecs' <- assembleClauses ldecs
        let sc' = extendScope ldecs' sc
        bnds <- elabLocDecls sc' ldecs'
        body' <- elabExpr sc' `traverse` body
        return $ T.LetE bnds body'
elabExpr sc (P.CaseE match alts) = do
        match' <- elabExpr sc `traverse` match
        alts' <- forM alts $ \(pat, body) -> do
                pat' <- elabPat sc `traverse` pat
                body' <- elabExpr (extendScope pat sc) `traverse` body
                return (pat', body')

        return $ T.CaseE match' alts'
elabExpr sc (P.AnnE exp ann_ty) = T.AnnE <$> elabExpr sc `traverse` exp <*> elabType sc (unLoc ann_ty)
elabExpr _ P.FactorE{} = unreachable "fixity resolution failed"

elabPat :: (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) => Scope -> P.Pat -> m T.Pat
elabPat sc (P.ConP con pats) = do
        con' <- scoping con sc
        pats' <- mapM (elabPat sc `traverse`) pats
        return $ T.ConP con' pats'
elabPat _ (P.VarP var) = return $ T.VarP var
elabPat _ P.WildP = return T.WildP
elabPat sc (P.BinP left op right) = do
        left' <- elabPat sc `traverse` left
        op' <- scoping op sc
        right' <- elabPat sc `traverse` right
        return $ T.ConP op' [left', right']
elabPat sc (P.AnnP pat ann_ty) = T.AnnP <$> elabPat sc `traverse` pat <*> elabType sc (unLoc ann_ty)
elabPat _ P.FactorP{} = unreachable "fixity resolution failed"

elabType ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Scope ->
        P.Type ->
        m T.Type
elabType sc (P.VarT var) = do
        var' <- scoping var sc
        return $ T.VarT (T.BoundTv var')
elabType sc (P.ConT con) = T.ConT <$> scoping con sc
elabType sc (P.ArrT arg res) = T.ArrT <$> (elabType sc `traverse` arg) <*> (elabType sc `traverse` res)
elabType sc (P.AllT vars body) = do
        paramNamesUnique vars
        qnts <- mapM (\id -> do kv <- newKnVar; return (T.BoundTv id, kv)) vars
        body' <- elabType (extendScope vars sc) `traverse` body
        return $ T.AllT qnts body'
elabType sc (P.AppT fun arg) = T.AppT <$> elabType sc `traverse` fun <*> elabType sc `traverse` arg
elabType sc (P.BinT left op right) = do
        left' <- elabType sc `traverse` left
        op' <- scoping op sc
        right' <- elabType sc `traverse` right
        return $ T.AppT (sL left' op $ T.AppT (L (getLoc op) (T.ConT op')) left') right'
elabType _ P.FactorT{} = unreachable "fixity resolution failed"

elabLocDecls ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        Scope ->
        [P.LLocDecl] ->
        m [T.Bind 'T.Untyped]
elabLocDecls sc ldecs = do
        (bnds, spcs) <- execWriterT $ forM ldecs $ \case
                L _ (P.FunSpecD id ty) -> do
                        id' <- scoping id sc
                        ty' <- elabType sc `traverse` ty
                        tell ([], [(id', ty')])
                L _ (P.FunBindD id clses) -> do
                        clses' <- mapM (elabClause sc) clses
                        tell ([(id, clses')], [])
                L _ P.FixityD{} -> unreachable "deleted by Nicifier"
        forM bnds $ \(id, clses) -> case lookup id spcs of
                Just ty -> return (T.Bind (id, Just ty) clses)
                _ -> return (T.Bind (id, Nothing) clses)

assembleClauses :: MonadThrow m => [P.LLocDecl] -> m [P.LLocDecl]
assembleClauses = assemble . partition
    where
        assemble :: MonadThrow m => [[P.LLocDecl]] -> m [P.LLocDecl]
        assemble [] = return []
        assemble (bnds@(L _ (P.FunBindD id _) : _) : rest) = do
                let clses = [(psi, ei) | L _ (P.FunBindD _ [(psi, ei)]) <- bnds]
                let spn = mconcat $ [spi | L spi P.FunBindD{} <- bnds]
                (L spn (P.FunBindD id clses) :) <$> assemble rest
        assemble (ldecs : rest) = (ldecs ++) <$> assemble rest
        partition :: [P.LLocDecl] -> [[P.LLocDecl]]
        partition =
                List.groupBy $ curry $ \case
                        (L _ (P.FunBindD id1 _), L _ (P.FunBindD id2 _)) -> nameIdent id1 == nameIdent id2
                        _ -> False
elabClause ::
        (MonadReader env m, HasUniq env, MonadIO m, MonadThrow m) =>
        Scope ->
        P.Clause ->
        m (T.Clause 'T.Untyped)
elabClause sc (pats, exp) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat sc `traverse`) pats
        exp' <- elabExpr (extendScope pats sc) `traverse` exp
        return (pats', exp')

elabDecl ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Scope ->
        P.LTopDecl ->
        m (T.TypDefn 'T.Untyped)
elabDecl sc (L _ (P.DataD id params ctors)) = do
        paramNamesUnique params
        dataConUnique $ map fst ctors
        mapM_ (dataConType id) ctors
        qnts <- mapM (\p -> (T.BoundTv p,) <$> newKnVar) params
        ctors' <- forM ctors $
                \(con, ty) -> (con,) <$> elabType (extendScope params sc) `traverse` ty
        return $ T.DatDefn id qnts ctors'
elabDecl _ _ = unreachable "data type definition required"

elabTopDecls ::
        (MonadReader e m, HasUniq e, MonadIO m, MonadThrow m) =>
        Scope ->
        [P.LTopDecl] ->
        m [T.Defn 'T.Untyped]
elabTopDecls sc tdecs = do
        let (tdecs', ldecs) = groupDecl tdecs
        let sc' = extendScope tdecs' sc
        tdefs <- mapM (elabDecl sc') tdecs'
        ldecs' <- assembleClauses ldecs
        let sc'' = extendScope ldecs' sc'
        mapM_ (checkNumArgs . unLoc) ldecs'
        binds <- elabLocDecls sc'' ldecs'
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
        catchErrors $ elabTopDecls mempty tdecs

psToTypExpr :: PlatoMonad m => P.LExpr -> m (T.LExpr 'T.Untyped)
psToTypExpr exp = do
        runReaderT (elabExpr mempty `traverse` exp) =<< getContext =<< ask