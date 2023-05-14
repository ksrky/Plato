{-# LANGUAGE TupleSections #-}

module Plato.Typing (typing) where

import Control.Exception.Safe
import Control.Monad.Reader

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Monad
import Plato.Typing.Tc

-- TypeSpec => ValueSpec => ModuleBind => TypeBind => ValueBind
typing ::
        (MonadReader env m, HasEnv env, HasUniq env, MonadThrow m, MonadIO m) =>
        [Decl] ->
        m [Decl]
typing (d@(SpecDecl (TypeSpec id kn)) : decs) = do
        decs' <- local (modifyEnv $ extend id kn) $ typing decs
        return $ d : decs'
typing (SpecDecl (ValSpec id ty) : decs) = do
        ty' <- checkKindStar ty
        local (modifyEnv $ extend id ty') $ typing decs
{-typing (BindDecl (TypeBind id _ ty) : decs) = do
        kn <- find id =<< getEnv =<< ask -- tmp: zonking
        decs' <- typing decs
        return $ BindDecl (TypeBind id (Just kn) ty) : decs'
typing (BindDecl (DataBind id fields) : decs) = do
        fields' <- mapM (\(con, ty) -> (con,) <$> checkKindStar ty) fields
        decs' <- local (modifyEnv $ extendList fields') $ typing decs
        return $ BindDecl (DataBind id fields') : decs'-}
typing (BindDecl (ValBind id _ exp) : decs) = do
        ty <- unLoc <$> (checkKindStar =<< find id =<< getEnv =<< ask) -- tmp: zonking?
        exp' <- checkType exp ty
        decs' <- typing decs
        return $ BindDecl (ValBind id (Just ty) exp') : decs'
typing _ = undefined