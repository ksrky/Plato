{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Plato.PsToTyp where

import Control.Exception.Safe
import Control.Monad.Reader

import Control.Monad.Writer
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Monad

elabExpr :: (MonadReader env m, HasUniq env, MonadIO m) => P.Expr -> m T.Expr
elabExpr (P.VarE id) = return $ T.VarE id
elabExpr (P.AppE fun arg) = T.AppE <$> elabExpr `traverse` fun <*> elabExpr `traverse` arg
elabExpr (P.LamE pats body) = do
        body' <- elabExpr `traverse` body
        varpats <- mapM (\p -> (,elabPat <$> p) <$> newVarIdent) pats
        let patlam (v, p) e = sL p e $ T.AbsE v Nothing $ sL p e $ T.CaseE (noLoc $ T.VarE v) [(p, e)]
        return $ unLoc $ foldr patlam body' varpats
elabExpr (P.LetE decs body) = do
        (bnds, spcs) <- elabFunDecls decs
        body' <- elabExpr `traverse` body
        return $ T.LetE bnds spcs body'

elabFunDecls :: -- tmp: type signature in let binding
        (MonadReader env m, HasUniq env, MonadIO m) =>
        [P.LFunDecl] ->
        m ([(Ident, [T.Clause])], [(Ident, T.LType)])
elabFunDecls fdecs = execWriterT $ forM fdecs $ \case
        L _ (P.FunSpec id ty) -> do
                ty' <- elabType `traverse` ty
                tell ([], [(id, ty')])
        L _ (P.FunBind id clses) -> do
                clses' <- mapM elabClause clses
                -- let body = L (getLoc clses) $ T.ClauseE Nothing clses'
                tell ([(id, clses')], [])

elabPat :: P.Pat -> T.Pat
elabPat (P.ConP con pats) = T.ConP con (map (elabPat <$>) pats)
elabPat (P.VarP var) = T.VarP var
elabPat P.WildP = T.WildP

elabType :: (MonadReader env m, HasUniq env, MonadIO m) => P.Type -> m T.Type
elabType (P.VarT path) = return $ T.VarT (T.BoundTv path)
elabType (P.ConT con) = return $ T.ConT con
elabType (P.ArrT arg res) = T.ArrT <$> (elabType `traverse` arg) <*> (elabType `traverse` res)
elabType (P.AllT qnts body) = do
        kv <- newKnVar
        T.AllT (map (\id -> (T.BoundTv id, kv)) qnts) <$> elabType `traverse` body
elabType (P.AppT fun arg) = T.AppT <$> elabType `traverse` fun <*> elabType `traverse` arg

elabClause :: (MonadReader env m, HasUniq env, MonadIO m) => P.Clause -> m T.Clause
elabClause (pats, exp) = do
        exp' <- elabExpr `traverse` exp
        return (map (elabPat <$>) pats, exp')

elabFunDecl :: (MonadReader env m, HasUniq env, MonadIO m) => P.FunDecl -> m T.Decl
elabFunDecl (P.FunSpec id ty) = do
        ty' <- elabType `traverse` ty
        return $ T.SpecDecl (T.ValSpec id ty')
elabFunDecl (P.FunBind id clses) = do
        clses' <- mapM elabClause clses
        -- let body = L (getLoc clses) $ T.ClauseE Nothing clses'
        return $ T.BindDecl (T.FunBind id clses')

elabDecl :: (MonadReader env m, HasUniq env, MonadIO m) => P.Decl -> m [T.Decl]
elabDecl (P.DataD id params constrs) = do
        qnts <- mapM (\p -> do kv <- newKnVar; return (T.BoundTv p, kv)) params
        constrs' <- mapM (\(con, ty) -> (con,) <$> (elabType `traverse` ty)) constrs
        sig <- newKnVar
        return $ T.SpecDecl (T.TypSpec id sig) : [T.BindDecl (T.DatBind id qnts constrs')]
elabDecl (P.FuncD fundec) = (:) <$> elabFunDecl fundec <*> pure []

elabTopDecl :: (MonadReader env m, HasUniq env, MonadIO m) => P.TopDecl -> m [T.Decl]
elabTopDecl = elabDecl

ps2typ :: (PlatoMonad m, MonadThrow m) => P.Program -> m T.Program
ps2typ tdecs = do
        decs <- concat <$> mapM (elabTopDecl . unLoc) tdecs
        return (decs, [])