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
import Data.Graph
import Data.List qualified as List
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.PsToTyp.Scoping
import Plato.PsToTyp.SynRstrc
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Syntax.Typing.Helper
import Plato.Typing.Linearize

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
        argPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        body' <- local (extendScope pats) $ elabExpr `traverse` body
        let patlam :: T.LExpr 'T.Untyped -> T.LPat -> m (T.LExpr 'T.Untyped)
            patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id Nothing e
            patlam e p = do
                v <- labelVarId "pl"
                return $ sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        unLoc <$> foldM patlam body' (reverse pats')
elabExpr (P.LetE ldecs body) = do
        mapM_ (checkNumArgs . unLoc) ldecs
        let ldecs' = assembleClauses ldecs
        local (extendScope ldecs') $ do
                bnds <- elabLocDecls ldecs'
                body' <- elabExpr `traverse` body
                return $ T.LetE (CyclicSCC bnds) body'
elabExpr (P.CaseE match alts) = do
        match' <- elabExpr `traverse` match
        alts' <- forM alts $ \(pat, body) -> do
                pat' <- elabPat `traverse` pat
                body' <- local (extendScope pat) $ elabExpr `traverse` body
                return (pat', body')

        return $ T.CaseE match' alts'
elabExpr P.FactorE{} = unreachable "fixity resolution failed"

elabPat :: (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) => P.Pat -> m T.Pat
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
        argNamesUnique vars
        qnts <- mapM (\id -> do kv <- newKnVar; return (T.BoundTv id, kv)) vars
        body' <- local (extendScope vars) $ elabType `traverse` body
        return $ T.AllT qnts body'
elabType (P.AppT fun arg) = T.AppT <$> elabType `traverse` fun <*> elabType `traverse` arg
elabType (P.BinT left op right) = do
        left' <- elabType `traverse` left
        op' <- scoping op
        right' <- elabType `traverse` right
        return $ T.AppT (sL left' op $ T.AppT (L (getLoc op) (T.ConT op')) left') right'
elabType P.FactorT{} = unreachable "fixity resolution failed"

elabLocDecls ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        [P.LLocDecl] ->
        m [T.Bind 'T.Untyped]
elabLocDecls ldecs = do
        (bnds, spcs) <- execWriterT $ forM ldecs $ \case
                L _ (P.FunSpecD id ty) -> do
                        id' <- scoping id
                        ty' <- lift $ elabType `traverse` ty
                        tell ([], [(id', ty')])
                L _ (P.FunBindD id cls) -> do
                        cls' <- lift $ mapM elabClause cls
                        tell ([(id, L (getLoc cls') (T.ClauseE cls'))], [])
                L _ P.FixityD{} -> unreachable "deleted by Nicifier"
        forM bnds $ \(id, exp) -> case lookup id spcs of
                Just ty -> return (T.Bind (id, Just ty) exp)
                _ -> return (T.Bind (id, Nothing) exp)

assembleClauses :: [P.LLocDecl] -> [P.LLocDecl]
assembleClauses = assemble . partition
    where
        assemble :: [[P.LLocDecl]] -> [P.LLocDecl]
        assemble [] = []
        assemble (bnds@(L _ (P.FunBindD id _) : _) : rest) = do
                let clses = [(psi, ei) | L _ (P.FunBindD _ [(psi, ei)]) <- bnds]
                    spn = mconcat $ [spi | L spi P.FunBindD{} <- bnds]
                 in L spn (P.FunBindD id clses) : assemble rest
        assemble (ldecs : rest) = ldecs ++ assemble rest
        partition :: [P.LLocDecl] -> [[P.LLocDecl]]
        partition =
                List.groupBy $ curry $ \case
                        (L _ (P.FunBindD id1 _), L _ (P.FunBindD id2 _)) -> nameIdent id1 == nameIdent id2
                        _ -> False
elabClause ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        P.Clause ->
        m (T.Clause 'T.Untyped)
elabClause (pats, exp) = do
        argPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        exp' <- local (extendScope pats) $ elabExpr `traverse` exp
        return (pats', exp')

elabDecl ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        P.LTopDecl ->
        m (T.TypDefn 'T.Untyped)
elabDecl (L _ (P.DataD id params ctors)) = do
        argNamesUnique params
        dataConUnique $ map fst ctors
        mapM_ (dataConType id) ctors
        qnts <- mapM (\p -> (T.BoundTv p,) <$> newKnVar) params
        ctors' <- local (extendScope params) $
                forM ctors $
                        \(con, ty) -> (con,) <$> elabType `traverse` ty
        return $ T.DatDefn id qnts ctors'
elabDecl _ = unreachable "data type definition required"

elabTopDecls ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        [P.LTopDecl] ->
        m ([T.Defn 'T.Untyped], e)
elabTopDecls tdecs = do
        let (tdecs', ldecs) = groupDecl tdecs
        local (extendScope tdecs') $ do
                tdefs <- mapM elabDecl tdecs'
                let ldecs' = assembleClauses ldecs
                local (extendScope ldecs') $ do
                        mapM_ (checkNumArgs . unLoc) ldecs'
                        binds <- elabLocDecls ldecs'
                        asks (linearizeTop [T.TypDefn (CyclicSCC tdefs), T.ValDefn (CyclicSCC binds)],)
    where
        groupDecl :: [P.LTopDecl] -> ([P.LTopDecl], [P.LLocDecl])
        groupDecl decs = execWriter $ forM decs $ \dec -> case dec of
                L _ P.DataD{} -> tell ([dec], [])
                L sp (P.LocalD ld) -> tell ([], [L sp ld])

-----------------------------------------------------------
-- psToTyp
-----------------------------------------------------------

psToTyp :: PlatoMonad m => [P.LTopDecl] -> m (T.Prog 'T.Untyped)
psToTyp = catchErrors . updateContext . elabTopDecls

psToTypExpr ::
        (HasCallStack, MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        P.LExpr ->
        m (T.LExpr 'T.Untyped)
psToTypExpr = traverse elabExpr