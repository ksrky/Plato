{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDecls, typing) where

import Control.Exception.Safe
import Control.Monad.Reader

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Monad
import Plato.Typing.Tc
import Plato.Typing.Zonking

typingDecls ::
        (MonadReader env m, HasTypEnv env, HasConEnv env, HasUniq env, MonadCatch m, MonadIO m) =>
        [Decl 'Untyped] ->
        m [Decl 'Typed]
typingDecls decs = typingDecls' decs >>= mapM zonkDecl

typingDecls' ::
        (MonadReader env m, HasTypEnv env, HasConEnv env, HasUniq env, MonadCatch m, MonadIO m) =>
        [Decl 'Untyped] ->
        m [Decl 'Typed]
typingDecls' [] = return []
typingDecls' (SpecDecl (TypSpec id kn) : decs) = do
        decs' <- local (modifyEnv $ extend id kn) $ typingDecls' decs
        return $ SpecDecl (TypSpec id kn) : decs'
typingDecls' (DefnDecl (DatDefn id params constrs) : decs) = do
        let extendEnv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        local (modifyEnv extendEnv) $ mapM_ (checkKindStar . snd) constrs
        let kn = foldr (\(_, kn1) kn2 -> ArrK kn1 kn2) StarK params
        decs' <-
                local (modifyEnv $ extendList $ map (\(con, ty) -> (con, AllT params ty)) constrs) $
                        local (extendConEnv id constrs) $
                                typingDecls' decs
        return $ DefnDecl (DatDefnok id kn params constrs) : decs'
typingDecls' (DefnDecl (TypDefn id ty) : decs) = do
        kn <- zonkKind =<< find id =<< getEnv =<< ask
        checkKind ty kn
        decs' <- typingDecls' decs
        return $ DefnDecl (TypDefn id ty) : decs'
typingDecls' (SpecDecl (ValSpec id ty) : decs) = do
        checkKindStar ty
        decs' <- local (modifyEnv $ extend id ty) $ typingDecls' decs
        return $ SpecDecl (ValSpec id ty) : decs'
typingDecls' (DefnDecl (FunDefn id clauses) : decs) = do
        sigma <- zonkType =<< find id =<< getEnv =<< ask
        exp <- checkClauses clauses sigma
        decs' <- typingDecls' decs
        return $ DefnDecl (FunDefnok id exp) : decs'

typing :: (PlatoMonad m, MonadCatch m) => Program 'Untyped -> m (Program 'Typed)
typing decs = do
        ctx <- initContext
        prog <- runReaderT (typingDecls decs) ctx
        uniq <- liftIO $ readUniq ctx
        setUniq uniq =<< ask
        return prog