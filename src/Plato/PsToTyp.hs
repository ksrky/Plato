{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Plato.PsToTyp where

import Control.Exception.Safe
import Control.Monad.Reader
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Parsing qualified as P
import Plato.Syntax.Typing qualified as T
import Plato.Typing.Monad

elabExpr :: P.Expr -> T.Expr
elabExpr (P.VarE id) = T.VarE id
elabExpr (P.AppE fun arg) = T.AppE (elabExpr <$> fun) (elabExpr <$> arg)
elabExpr (P.LamE pats body) = undefined
-- unLoc $ foldr (\p e -> sL p e $ T.PAbsE p Nothing e) (elabExpr <$> body) (map (elabPat <$>) pats)
elabExpr (P.LetE decs body) = do
        undefined

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

elabFunDecls :: [P.FunDecl] -> ([(Ident, T.LExpr)], [(Ident, T.Type)])
elabFunDecls _ = undefined

elabFunDecl :: (HasCallStack, MonadReader env m, HasUniq env, MonadIO m) => P.FunDecl -> m T.Decl
elabFunDecl (P.FunSpec id ty) = do
        ty' <- elabType `traverse` ty
        return $ T.SpecDecl (T.ValSpec id ty')
elabFunDecl (P.FunBind id [] exp) = do
        let exp' = elabExpr <$> exp
        return $ T.BindDecl (T.ValBind id exp')
elabFunDecl P.FunBind{} = unreachable ""

elabDecl :: (MonadReader env m, HasUniq env, MonadIO m) => P.Decl -> m [T.Decl]
elabDecl (P.DataD id params constrs) = do
        qnts <- mapM (\p -> do kv <- newKnVar; return (T.BoundTv p, kv)) params
        let quantify :: T.LType -> T.LType
            quantify ty = L (getLoc ty) $ T.AllT qnts ty
        constrs' <- mapM (\(con, ty) -> (con,) <$> (quantify <$> elabType `traverse` ty)) constrs
        sig <- newKnVar
        return $ T.SpecDecl (T.TypSpec id sig) : [T.BindDecl (T.DatBind id qnts constrs')]
elabDecl (P.FuncD (P.FunSpec id sig)) = do
        sig' <- elabType `traverse` sig
        return [T.SpecDecl (T.ValSpec id sig')]
elabDecl (P.FuncD fundec) = (:) <$> elabFunDecl fundec <*> pure []

elabTopDecl :: (MonadReader env m, HasUniq env, MonadIO m) => P.TopDecl -> m [T.Decl]
elabTopDecl (P.Decl dec) = elabDecl (unLoc dec)
elabTopDecl (P.Eval _) = undefined

ps2typ :: (PlatoMonad m, MonadThrow m) => P.Program -> m T.Program
ps2typ tdecs = do
        decs <- concat <$> mapM (elabTopDecl . unLoc) tdecs
        return (decs, [])
