{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDecls, typing) where

import Control.Exception.Safe
import Control.Monad.Reader
import System.Log.Logger

import Plato.Common.Error
import Plato.Common.Uniq
import Plato.Driver.Logger
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
typingDecls decs = typingDecls' decs >>= mapM zonk

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
        kn <- zonk =<< find id =<< getEnv =<< ask
        checkKind ty kn
        decs' <- typingDecls' decs
        return $ DefnDecl (TypDefn id ty) : decs'
typingDecls' (SpecDecl (ValSpec id ty) : decs) = do
        checkKindStar ty
        decs' <- local (modifyEnv $ extend id ty) $ typingDecls' decs
        return $ SpecDecl (ValSpec id ty) : decs'
typingDecls' (DefnDecl (FunDefn id clauses) : decs) = do
        liftIO $ debugM platoLog $ "Start type checking of function '" ++ show id ++ "'"
        sigma <- zonk =<< find id =<< getEnv =<< ask
        exp <- checkClauses clauses sigma
        decs' <- typingDecls' decs
        return $ DefnDecl (FunDefnok id exp) : decs'

typing :: PlatoMonad m => Program 'Untyped -> m (Program 'Typed)
typing decs = do
        ctx <- initContext
        prog <-
                runReaderT (typingDecls decs) ctx
                        `catches` [ Handler $ \e@LocErr{} -> liftIO (print e) >> return []
                                  , Handler $ \e@PlainErr{} -> liftIO (print e) >> return []
                                  , Handler $ \(e :: SomeException) -> liftIO (print e) >> return []
                                  ]
        uniq <- liftIO $ readUniq ctx
        setUniq uniq =<< ask
        return prog