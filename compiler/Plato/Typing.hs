{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (typingDecls, typingDefns, typing, typingExpr) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer

import Plato.Common.Error
import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Syntax.Typing
import Plato.Typing.Env
import Plato.Typing.Kc
import Plato.Typing.Tc
import Plato.Typing.Zonking

typingDecls ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Decl 'Untyped] ->
        m ([Decl 'Typed], e)
typingDecls decs = do
        (env, decs') <- runWriterT $ typingDecls' decs
        (,env) <$> mapM zonk decs'

typingDecls' ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Decl 'Untyped] ->
        WriterT [Decl 'Typed] m e
typingDecls' [] = ask
typingDecls' (SpecDecl (TypSpec id kn) : decs) = do
        tell [SpecDecl (TypSpec id kn)]
        local (modifyTypEnv $ extend id kn) $ typingDecls' decs
typingDecls' (DefnDecl (DatDefn id params constrs) : decs) = do
        let extenv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        local (modifyTypEnv extenv) $ mapM_ (checkKindStar . snd) constrs
        let kn = foldr (\(_, kn1) kn2 -> ArrK kn1 kn2) StarK params
        tell [DefnDecl (DatDefnok id kn params constrs)]
        local (modifyTypEnv $ extendList $ map (\(con, ty) -> (con, AllT params ty)) constrs) $
                local (extendConEnv id (map fst params) constrs) $
                        typingDecls' decs
typingDecls' (DefnDecl (TypDefn id ty) : decs) = do
        kn <- zonk =<< find id =<< asks getTypEnv
        checkKind ty kn
        tell [DefnDecl (TypDefn id ty)]
        typingDecls' decs
typingDecls' (SpecDecl (ValSpec id ty) : decs) = do
        checkKindStar ty
        tell [SpecDecl (ValSpec id ty)]
        local (modifyTypEnv $ extend id ty) $ typingDecls' decs
typingDecls' (DefnDecl (FunDefn id clauses) : decs) = do
        sigma <- zonk =<< find id =<< asks getTypEnv
        exp <- checkDefn clauses sigma
        tell [DefnDecl (FunDefnok id exp)]
        typingDecls' decs

typingDefns ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Defn' 'Untyped] ->
        WriterT [Defn' 'Typed] m e
typingDefns [] = undefined
typingDefns (ValDefn' binds : rest) = do
        binds' <- tcBinds binds
        tell [ValDefn' binds']
        let sig = map (\(Bind' idty _) -> idty) binds'
        local (modifyTypEnv $ extendList sig) $ typingDefns rest
typingDefns (TypDefn' tdefs : rest) = do
        tdefs' <- kcTypDefns tdefs
        tell [TypDefn' tdefs']
        let datsig = map (\(DatDefn'' idkn _ _) -> idkn) tdefs'
            ctors = concatMap (\(DatDefn'' _ _ ctors) -> ctors) tdefs'
            extce env = foldr (\(DatDefn'' (id, _) qns ctors) -> extendConEnv id (map fst qns) ctors) env tdefs'
        local (modifyTypEnv $ extendList datsig . extendList ctors) $
                local (modifyConEnv extce) $
                        typingDefns rest

-----------------------------------------------------------
-- typing
-----------------------------------------------------------
typing :: PlatoMonad m => Program 'Untyped -> m (Program 'Typed)
typing decs = catchErrors $ updateContext (typingDecls decs)

typingExpr :: PlatoMonad m => LExpr 'Untyped -> m (LExpr 'Typed)
typingExpr exp = do
        (exp', _) <- runReaderT (inferType exp) =<< getContext =<< ask
        return exp'