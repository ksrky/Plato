{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Plato.PsToTyp (ps2typ) where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.List.NonEmpty qualified as NE

import Plato.Common.Error
import Plato.Common.Global
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Monad

elabExpr ::
        (MonadReader env m, HasUnique env, MonadThrow m, MonadIO m) =>
        P.LExpr ->
        m T.LExpr
elabExpr (L sp exp) =
        L sp <$> case exp of
                P.VarE path -> return $ T.VarE path
                P.AppE fun arg -> T.AppE <$> elabExpr fun <*> elabExpr arg
                P.OpE lhs op rhs -> do
                        lhs' <- elabExpr lhs
                        let fun = L (getLoc op) $ T.VarE op
                        rhs' <- elabExpr rhs
                        return $ T.AppE (sL fun lhs (T.AppE fun lhs')) rhs'
                P.LamE [] -> do
                        -- Note: absurd pattern
                        id <- newVarIdent
                        let match = (noLoc $ T.VarE $ PIdent id, Nothing) NE.:| []
                        return $ T.AbsE id Nothing (noLoc $ T.MatchE match [])
                P.LamE alts@(hd : _) -> do
                        -- Note: pattern rows are not empty
                        ids <- mapM (const newVarIdent) (NE.fromList $ fst hd)
                        let matches = NE.map (\id -> (noLoc $ T.VarE $ PIdent id, Nothing)) ids
                        alts' <- mapM (\(ps, e) -> (NE.fromList $ map elabPat ps,) <$> elabExpr e) alts
                        return $ unLoc $ foldr (\id -> noLoc . T.AbsE id Nothing) (noLoc $ T.MatchE matches alts') ids
                P.LetE decs body -> do
                        decs' <- concat <$> mapM elabDecl decs
                        body' <- elabExpr body
                        return $ T.LetE decs' body'
                P.CaseE match alts -> do
                        match' <- elabExpr match
                        alts' <- mapM (\(p, e) -> (elabPat p NE.:| [],) <$> elabExpr e) alts
                        return $ T.MatchE ((match', Nothing) NE.:| []) alts'
                P.FactorE{} -> unreachable "Plato.PsToTyp.elabExpr passed FactorE"

elabPat :: P.LPat -> T.LPat
elabPat (L sp (P.ConP con pats)) = L sp (T.ConP con (map elabPat pats))
elabPat (L sp (P.VarP id)) = L sp (T.VarP id)
elabPat (L sp P.WildP) = L sp T.WildP

elabType :: P.LType -> T.LType
elabType (L sp ty) =
        L sp $ case ty of
                P.VarT path -> T.VarT (T.BoundTv path)
                P.ConT con -> T.ConT con
                P.AppT fun arg -> T.AppT (elabType fun) (elabType arg)
                P.ArrT arg res -> T.ArrT (elabType arg) (elabType res)
                P.AllT qnts body ->
                        T.AllT (map (\id -> (T.BoundTv id, Nothing)) qnts) (elabType body)

elabDecl ::
        (MonadReader env m, HasUnique env, MonadThrow m, MonadIO m) =>
        P.LDecl ->
        m [T.Decl]
elabDecl (L _ dec) = case dec of
        P.OpenD path -> return [T.OpenDecl path]
        P.FixityD{} -> return []
        P.ModuleD id mod -> do
                modbnd <- T.ModuleBind id <$> elabModule mod
                return [T.BindDecl modbnd]
        {-P.DataD id params constrs -> do
                let constrs' = map (\(con, ty) -> (con, elabType ty)) constrs
                constrs'' <- inferDataKind params constrs'
                let condecs = map (T.SpecDecl . uncurry T.ValueSpec) constrs''
                kn <- getDataType id
                return $ T.SpecDecl (T.TypeSpec id kn) : condecs-}
        P.DataD id params constrs -> do
                let quantify :: [Ident] -> T.LType -> T.LType
                    quantify params ty = foldr (\id -> L (getLoc ty) . T.AbsT id Nothing) ty params
                let constrs' = map (\(con, ty) -> (con, quantify params $ elabType ty)) constrs
                let condecs = map (T.SpecDecl . uncurry T.ValueSpec) constrs'
                kv <- newKnVar
                return $ T.SpecDecl (T.TypeSpec id kv) : T.BindDecl (T.DataBind id constrs') : condecs
        P.FuncSigD id ty -> return [T.SpecDecl (T.ValueSpec id (elabType ty))]
        P.FuncD id [] body -> do
                body' <- elabExpr body
                return [T.BindDecl (T.ValueBind id Nothing body')]
        P.FuncD{} -> undefined

elabModule ::
        (MonadReader env m, HasUnique env, MonadThrow m, MonadIO m) =>
        P.LModule ->
        m T.LModule
elabModule (L sp (P.Module decs)) = L sp <$> (T.Module . concat <$> mapM elabDecl decs)

elabTopDecl :: P.TopDecl -> T.Decl
elabTopDecl = undefined

ps2typ ::
        (MonadReader env m, HasUnique env, MonadThrow m, MonadIO m) =>
        P.Program ->
        m T.Program
ps2typ = undefined