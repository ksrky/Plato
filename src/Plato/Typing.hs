{-# LANGUAGE TupleSections #-}

module Plato.Typing (typing) where

import Control.Exception.Safe
import Control.Monad.Reader

import Plato.Common.Global
import Plato.Common.Location
import Plato.KindCheck.Kc
import Plato.Syntax.Typing
import Plato.TypeCheck.Tc
import Plato.Typing.Env
import Plato.Typing.Monad

getSig :: LModule -> Signature
getSig = undefined -- TODO

-- TypeSpec => ValueSpec => ModuleBind => TypeBind => ValueBind
typing ::
        (MonadReader env m, HasEnv env, HasUnique env, MonadThrow m, MonadIO m) =>
        [Decl] ->
        m [Decl]
typing (BindDecl (ModuleBind id mod) : decs) = do
        -- TODO: Scope semantics
        mod' <- typingModule `traverse` mod
        let sig = getSig mod'
        decs' <- local (modifyEnv $ extend id sig) $ typing decs
        return (BindDecl (ModuleBind id mod') : decs')
typing (d@(SpecDecl (TypeSpec id kn)) : decs) = do
        decs' <- local (modifyEnv $ extend id kn) $ typing decs
        return $ d : decs'
typing (SpecDecl (ValueSpec id ty) : decs) = do
        ty' <- checkKindStar ty
        local (modifyEnv $ extend id ty') $ typing decs
{-typing (BindDecl (TypeBind id _ ty) : decs) = do
        kn <- find id =<< getEnv =<< ask -- tmp: zonking
        decs' <- typing decs
        return $ BindDecl (TypeBind id (Just kn) ty) : decs'-}
typing (BindDecl (DataBind id fields) : decs) = do
        fields' <- mapM (\(con, ty) -> (con,) <$> checkKindStar ty) fields
        decs' <- local (modifyEnv $ extendList fields') $ typing decs
        return $ BindDecl (DataBind id fields') : decs'
typing (BindDecl (ValueBind id _ exp) : decs) = do
        ty <- unLoc <$> (checkKindStar =<< findId id =<< getEnv =<< ask) -- tmp: zonking?
        exp' <- checkType exp ty
        decs' <- typing decs
        return $ BindDecl (ValueBind id (Just ty) exp') : decs'
typing _ = undefined

typingModule ::
        (MonadReader env m, HasEnv env, HasUnique env, MonadThrow m, MonadIO m) =>
        Module ->
        m Module
typingModule (Module decs) = Module <$> typing decs
