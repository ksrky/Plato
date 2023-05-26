{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Plato.PsToTyp (
        elabExpr,
        elabPat,
        elabType,
        elabDecls,
        elabTopDecls,
        ps2typ,
) where

import Control.Exception.Safe
import Control.Monad.Reader
import Control.Monad.Writer
import Data.IORef (IORef)
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.PsToTyp.Scoping
import Plato.PsToTyp.SynRstrc
import Plato.PsToTyp.Utils
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Monad

elabExpr ::
        forall env m.
        (HasCallStack, MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) =>
        P.Expr ->
        m (T.Expr 'T.TcUndone)
elabExpr (P.VarE id) = T.VarE <$> scoping id
elabExpr (P.AppE fun arg) = T.AppE <$> elabExpr `traverse` fun <*> elabExpr `traverse` arg
elabExpr (P.OpE left op right) = do
        left' <- elabExpr `traverse` left
        op' <- scoping op
        right' <- elabExpr `traverse` right
        return $ T.AppE (sL left' op $ T.AppE (L (getLoc op) (T.VarE op')) left') right'
elabExpr (P.LamE pats body) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        body' <- local (extendListScope (allIdentsFromPats pats)) $ elabExpr `traverse` body
        let patlam :: T.LExpr 'T.TcUndone -> T.LPat -> m (T.LExpr 'T.TcUndone)
            patlam e p@(L _ (T.VarP id)) = return $ sL p e $ T.AbsE id e
            patlam e p = do
                v <- newVarIdent -- tmp: wild card pattern
                return $ sL p e $ T.AbsE v $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        unLoc <$> foldM patlam body' pats'
elabExpr (P.LetE fdecs body) = do
        env <- extendScopeFromSeq fdecs
        local (const env) $ do
                (bnds, spcs) <- elabFunDecls fdecs
                body' <- local (const env) $ elabExpr `traverse` body
                return $ T.LetE bnds spcs body'
elabExpr P.FactorE{} = unreachable "fixity resolution failed"

elabPat :: (MonadReader env m, HasScope env, MonadThrow m) => P.Pat -> m T.Pat
elabPat (P.ConP con pats) = do
        con' <- scoping con
        pats' <- mapM (elabPat `traverse`) pats
        return $ T.ConP con' pats'
elabPat (P.VarP var) = T.VarP <$> scoping var
elabPat P.WildP = return T.WildP

elabType :: (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) => P.Type -> m T.Type
elabType (P.VarT var) = do
        var' <- scoping var
        return $ T.VarT (T.BoundTv var')
elabType (P.ConT con) = T.ConT <$> scoping con
elabType (P.ArrT arg res) = T.ArrT <$> (elabType `traverse` arg) <*> (elabType `traverse` res)
elabType (P.AllT qnts body) = do
        paramNamesUnique qnts
        kv <- newKnVar
        body' <- elabType `traverse` body
        return $ T.AllT (map (\id -> (T.BoundTv id, kv)) qnts) body'
elabType (P.AppT fun arg) = T.AppT <$> elabType `traverse` fun <*> elabType `traverse` arg

elabFunDecls :: -- tmp: type signature in let binding
        (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) =>
        [P.LFunDecl] ->
        m ([(Ident, [T.Clause 'T.TcUndone])], [(Ident, T.LType)])
elabFunDecls fdecs = execWriterT $ forM fdecs $ \case
        L _ (P.FunSpec id ty) -> do
                ty' <- elabType `traverse` ty
                tell ([], [(id, ty')])
        L _ (P.FunBind id clses) -> do
                clses' <- mapM elabClause clses
                tell ([(id, clses')], [])
        L _ P.FixDecl{} -> unreachable "deleted by Nicifier"

elabClause :: (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) => P.Clause -> m (T.Clause 'T.TcUndone)
elabClause (pats, exp) = do
        paramPatsUnique pats
        pats' <- mapM (elabPat `traverse`) pats
        exp' <- elabExpr `traverse` exp
        return (pats', exp')

elabDecls :: (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) => [P.Decl] -> m [T.Decl 'T.TcUndone]
elabDecls [] = undefined
elabDecls (P.DataD id params constrs : decs) = do
        qnts <- mapM (\p -> do kv <- newKnVar; return (T.BoundTv p, kv)) params
        constrs' <- mapM (\(con, ty) -> (con,) <$> (elabType `traverse` ty)) constrs
        sig <- newKnVar
        decs' <- local (extendScope id) $ elabDecls decs
        return $ T.SpecDecl (T.TypSpec id sig) : T.BindDecl (T.DatBind id qnts constrs') : decs'
elabDecls (P.FuncD fundecs : decs) = do
        env <- extendScopeFromSeq fundecs
        local (const env) $ do
                (bnds, spcs) <- elabFunDecls fundecs
                decs' <- elabDecls decs
                let fundecs' = map (T.SpecDecl . uncurry T.ValSpec) spcs ++ map (T.BindDecl . uncurry T.FunBind) bnds
                return $ fundecs' ++ decs'

elabTopDecls :: (MonadReader env m, HasUniq env, HasScope env, MonadIO m, MonadThrow m) => [P.LTopDecl] -> m [T.Decl 'T.TcUndone]
elabTopDecls = elabDecls . map unLoc

data Context = Context {ctx_uniq :: IORef Uniq, ctx_scope :: Scope}

instance HasUniq Context where
        getUniq = return . ctx_uniq

instance HasScope Context where
        getScope (Context _ sc) = sc
        modifyScope f ctx = ctx{ctx_scope = f (ctx_scope ctx)}

ps2typ :: (PlatoMonad m, MonadThrow m) => P.Program -> m (T.Program 'T.TcUndone)
ps2typ tdecs = do
        uniq <- getUniq =<< ask
        decs <- runReaderT (elabTopDecls tdecs) (Context uniq initScope)
        return (decs, [])