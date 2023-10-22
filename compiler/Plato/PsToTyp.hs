{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTyp (psToTyp, psToTypExpr) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Graph
import Data.List                  qualified as List
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.PsToTyp.Resolver
import Plato.PsToTyp.Scoping
import Plato.PsToTyp.SynRstrc
import Plato.Syntax.Parsing       qualified as P
import Plato.Syntax.Typing        qualified as T
import Plato.Syntax.Typing.Helper

class Expandable a b where
    expand :: (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) => a -> WriterT [Ident] m [Tok b]

instance Expandable P.LExpr (T.LExpr 'T.Untyped) where
    expand (L _ (P.BinE lhs op rhs)) = do
        lhs' <- expand lhs
        op' <- scoping op
        tell [op']
        rhs' <- expand rhs
        return $ lhs' ++ [TOp op'] ++ rhs'
    expand exp = do
        exp' <- elabExpr `traverse` exp
        return [TTerm exp']

instance Expandable P.LPat T.LPat where
    expand (L _ (P.BinP lhs op rhs)) = do
        lhs' <- expand lhs
        op' <- scoping op
        rhs' <- expand rhs
        return $ lhs' ++ [TOp op'] ++ rhs'
    expand pat = do
        pat' <- elabPat `traverse` pat
        return [TTerm pat']

instance Expandable P.LType T.LType where
    expand (L _ (P.BinT lhs op rhs)) = do
        lhs' <- expand lhs
        op' <- scoping op
        tell [op']
        rhs' <- expand rhs
        -- TODO: infix declaration does not distinguish ConOp or TyConOp
        return $ lhs' ++ [TOp op'] ++ rhs'
    expand ty = do
        ty' <- elabType `traverse` ty
        return [TTerm ty']

elabExpr ::
    forall e m.
    (HasCallStack, MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.Expr -> WriterT [Ident] m (T.Expr 'T.Untyped)
elabExpr (P.VarE id) = do
    id' <- scoping id
    tell [id']
    return $ T.VarE id'
elabExpr (P.AppE fun arg) = T.AppE <$> elabExpr `traverse` fun <*> elabExpr `traverse` arg
elabExpr exp@P.BinE{} = do
    toks <- expand $ noLoc exp
    let elab :: Ident -> T.LExpr 'T.Untyped -> T.LExpr 'T.Untyped -> T.LExpr 'T.Untyped
        elab op l r = sL l r $ T.AppE (sL l op $ T.AppE (L (getLoc op) (T.VarE op)) l) r
    unLoc <$> parseTok elab toks
elabExpr (P.LamE pats body) = do
    argPatsUnique pats
    pats' <- mapM (elabPat `traverse`) pats
    body' <- local (extendScope pats) $ elabExpr `traverse` body
    let patlam :: T.LExpr 'T.Untyped -> T.LPat -> WriterT [Ident] m (T.LExpr 'T.Untyped)
        patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id Nothing e
        patlam e p = do
            v <- labelVarId "pl"
            return $ sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) Nothing [(p, e)]
    unLoc <$> foldM patlam body' (reverse pats')
elabExpr (P.LetE ldecs body) = do
    mapM_ (checkNumArgs . unLoc) ldecs
    let ldecs' = assembleClauses ldecs
    local (extendScope ldecs') $ do
        bnds <- elabLocDecls ldecs'
        body' <- elabExpr `traverse` body
        return $ unLoc $ foldr (\b e -> sL b e $ T.LetE b e) body' bnds
elabExpr (P.CaseE match alts) = do
    match' <- elabExpr `traverse` match
    alts' <- forM alts $ \(pat, body) -> do
        pat' <- elabPat `traverse` pat
        body' <- local (extendScope pat) $ elabExpr `traverse` body
        return (pat', body')
    return $ T.CaseE match' Nothing alts'
elabExpr (P.FactorE exp) = elabExpr (unLoc exp)

elabPat ::
    (HasCallStack, MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.Pat -> m T.Pat
elabPat (P.ConP con pats) = do
    con' <- scoping con
    pats' <- mapM (elabPat `traverse`) pats
    return $ T.ConP con' pats'
elabPat (P.VarP var) = return $ T.VarP var
elabPat P.WildP = return T.WildP
elabPat pat@P.BinP{} = do
    toks <- fmap fst $ runWriterT $ expand $ noLoc pat
    let elab :: Ident -> T.LPat -> T.LPat -> T.LPat
        elab op l r = sL l r $ T.ConP op [l, r]
    unLoc <$> parseTok elab toks
elabPat (P.AnnP pat ann_ty) = T.AnnP <$> elabPat `traverse` pat <*> (fst <$> runWriterT (elabType (unLoc ann_ty)))
elabPat (P.FactorP pat) = elabPat (unLoc pat)

elabType ::
    (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.Type -> WriterT [Ident] m T.Type
elabType (P.VarT var) = do
    var' <- scoping var
    return $ T.VarT (T.BoundTv var')
elabType (P.ConT con) = do
    con' <- scoping con
    tell [con']
    return $ T.ConT con'
elabType (P.ArrT arg res) = T.ArrT <$> elabType `traverse` arg <*> elabType `traverse` res
elabType (P.AllT vars body) = do
    argNamesUnique vars
    qnts <- mapM (\id -> do kv <- newKnVar; return (T.BoundTv id, kv)) vars
    body' <- local (extendScope vars) $ elabType `traverse` body
    return $ T.AllT qnts body'
elabType (P.AppT fun arg) = T.AppT <$> elabType `traverse` fun <*> elabType `traverse` arg
elabType ty@P.BinT{} = do
    toks <- expand $ noLoc ty
    let elab :: Ident -> T.LType -> T.LType -> T.LType
        elab op l r = sL l r $ T.AppT (sL l op $ T.AppT (L (getLoc op) (T.ConT op)) l) r
    unLoc <$> parseTok elab toks
elabType (P.FactorT ty) = elabType (unLoc ty)

elabLocDecls ::
    (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    [P.LLocDecl] -> WriterT [Ident] m [T.Block (T.XBind 'T.Untyped)]
elabLocDecls ldecs = do
    bnds <- sequence [ do   (cls', deps) <- runWriterT $ mapM elabClause cls
                            return (id, L (getLoc cls') (T.ClauseE cls'), deps)
                     | L _ (P.FunBindD id cls) <- ldecs ]
    spcs <- sequence [ do   id' <- scoping id
                            ty' <- elabType `traverse` ty
                            return (id', ty')
                     | L _ (P.FunSpecD id ty) <- ldecs ]
    graph <- forM bnds $ \(id, exp, deps) -> case lookup id spcs of
                Just ty -> return (sL id exp (T.Bind (id, Just ty) exp), stamp id, map stamp deps)
                _       -> return (sL id exp (T.Bind (id, Nothing) exp), stamp id, map stamp deps)
    return $ map sccToBlock (stronglyConnComp graph)

assembleClauses :: [P.LLocDecl] -> [P.LLocDecl]
assembleClauses ldecs = assemble $ partition ldecs
  where
    fixities = [(name, fix) | L _ (P.FixityD (L _ name) fix) <- ldecs]
    assemble :: [[P.LLocDecl]] -> [P.LLocDecl]
    assemble [] = []
    assemble (bnds@(L _ (P.FunBindD id _) : _) : rest) = do
        let clses = [(psi, ei) | L _ (P.FunBindD _ [(psi, ei)]) <- bnds]
            sp = mconcat $ [spi | L spi P.FunBindD{} <- bnds]
            id' = maybe id (setFixity id) (lookup (nameIdent id) fixities)
        L sp (P.FunBindD id' clses) : assemble rest
    assemble (ldecs : rest) = ldecs ++ assemble rest
    partition :: [P.LLocDecl] -> [[P.LLocDecl]]
    partition = List.groupBy $ curry $ \case
                    (L _ (P.FunBindD id1 _), L _ (P.FunBindD id2 _)) -> nameIdent id1 == nameIdent id2
                    _ -> False

elabClause ::
    (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.Clause -> WriterT [Ident] m (T.Clause 'T.Untyped)
elabClause (pats, exp) = do
    argPatsUnique pats
    pats' <- mapM (elabPat `traverse`) pats
    exp' <- local (extendScope pats) $ elabExpr `traverse` exp
    return (pats', exp')

elabDataDecl ::
    (HasCallStack, MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.LTopDecl ->
    m (T.XTypDefn 'T.Untyped, Uniq, [Uniq])
elabDataDecl (L sp (P.DataD id params ctors)) = do
    argNamesUnique params
    dataConUnique $ map fst ctors
    mapM_ (dataConType id) ctors
    qnts <- mapM (\p -> (T.BoundTv p,) <$> newKnVar) params
    (ctors', deps) <- runWriterT $ local (extendScope params) $ forM ctors $ \(con, ty) ->
                        (con,) <$> elabType `traverse` ty
    return (L sp (T.DatDefn id qnts ctors'), stamp id, map stamp deps)
elabDataDecl _ = unreachable "data type definition required"

elabDataDecls ::
    (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    [P.LTopDecl] -> m [T.Block (Located T.TypDefn)]
elabDataDecls decs = do
    graph <- mapM elabDataDecl decs
    return $ map sccToBlock (stronglyConnComp graph)

elabTopDecls ::
        (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
        [P.LTopDecl] -> m ([T.Defn 'T.Untyped], e)
elabTopDecls tdecs =
    local (extendScope tdecs') $ do
        tdefs <- elabDataDecls tdecs'
        let ldecs' = assembleClauses ldecs
        mapM_ (checkNumArgs . unLoc) ldecs'
        local (extendScope ldecs') $ do
            binds <- fmap fst $ runWriterT $ elabLocDecls ldecs'
            asks (map T.TypDefn tdefs ++ map T.ValDefn binds,)
  where
    fixities = [(name, fix) | L _ (P.LocalD (P.FixityD (L _ name) fix)) <- tdecs]
    groupDecl :: [P.LTopDecl] -> ([P.LTopDecl], [P.LLocDecl])
    groupDecl decs =
        execWriter $ forM decs $ \dec -> case dec of
            L sp (P.DataD id params ctors) -> do
                let id' = maybe id (setFixity id) (lookup (nameIdent id) fixities)
                ctors' <- forM ctors $ \(con, ty) -> do
                    let con' = maybe con (setFixity con) (lookup (nameIdent con) fixities)
                    return (con', ty)
                tell ([L sp (P.DataD id' params ctors')], [])
            L sp (P.LocalD ld) -> tell ([], [L sp ld])
    (tdecs', ldecs) = groupDecl tdecs

-----------------------------------------------------------
-- psToTyp
-----------------------------------------------------------
psToTyp :: (PlatoMonad m) => [P.LTopDecl] -> m (T.Prog 'T.Untyped)
psToTyp = catchErrors . updateContext . elabTopDecls

psToTypExpr ::
    (MonadReader e m, HasUniq e, HasScope e, MonadIO m, MonadThrow m) =>
    P.LExpr -> m (T.LExpr 'T.Untyped)
psToTypExpr = fmap fst . runWriterT . traverse elabExpr
