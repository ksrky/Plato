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
        (MonadReader env m, HasTypEnv env, HasUniq env, HasConEnv env, MonadThrow m, MonadIO m) =>
        [Decl 'TcUndone] ->
        m [Decl 'TcDone]
typingDecls [] = return []
typingDecls (SpecDecl (TypSpec id kn) : decs) = do
        decs' <- local (modifyEnv $ extend id kn) $ typingDecls decs
        return $ SpecDecl (TypSpec id kn) : decs'
typingDecls (SpecDecl (ValSpec id ty) : decs) = do
        checkKindStar ty
        decs' <- local (modifyEnv $ extend id ty) $ typingDecls decs
        return $ SpecDecl (ValSpec id ty) : decs'
typingDecls (BindDecl (DatBind id params constrs) : decs) = do
        let extendEnv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        local (modifyEnv extendEnv) $ mapM_ (checkKindStar . snd) constrs
        let kn = foldr (\(_, kn1) kn2 -> ArrK kn1 kn2) StarK params
        decs' <- local (modifyEnv $ extendList $ map (\(con, ty) -> (con, AllT params ty)) constrs) $ typingDecls decs
        return $ BindDecl (DatBindok id kn params constrs) : decs'
typingDecls (BindDecl (TypBind id ty) : decs) = do
        kn <- zonkKind =<< find id =<< getEnv =<< ask
        checkKind ty kn
        decs' <- local (modifyEnv $ extend id kn) $ typingDecls decs
        return $ BindDecl (TypBind id ty) : decs'
typingDecls (BindDecl (FunBind id clauses) : decs) = do
        sigma <- zonkType =<< find id =<< getEnv =<< ask
        exp <- checkClauses clauses sigma
        decs' <- local (modifyEnv $ extend id sigma) $ typingDecls decs
        return $ BindDecl (FunBindok id exp) : decs'

typing :: (PlatoMonad m, MonadThrow m) => Program 'TcUndone -> m (Program 'TcDone)
typing (decs, _exps) = do
        ctx <- initContext
        decs' <- runReaderT (typingDecls decs >>= mapM zonkDecl) ctx
        -- exptys <- runReaderT (mapM inferType exps) ctx'
        return (decs', []) -- tmp: evals