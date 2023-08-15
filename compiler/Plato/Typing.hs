{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (
        typingDecls,
        typing,
        typingExpr,
) where

import Control.Exception.Safe
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
        (env, decs) <- runWriterT $ typingDecls' decs
        (,env) <$> mapM zonk decs

typingDecls' ::
        (MonadReader e m, HasTypEnv e, HasConEnv e, HasUniq e, MonadCatch m, MonadIO m) =>
        [Decl 'Untyped] ->
        WriterT [Decl 'Typed] m e
typingDecls' [] = ask
typingDecls' (SpecDecl (TypSpec id kn) : decs) = do
        tell [SpecDecl (TypSpec id kn)]
        local (modifyTypEnv $ extend id kn) $ typingDecls' decs
typingDecls' (DefnDecl (DatDefn id params constrs) : decs) = do
        let extendEnv = extendList $ map (\(tv, kn) -> (unTyVar tv, kn)) params
        local (modifyTypEnv extendEnv) $ mapM_ (checkKindStar . snd) constrs
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
        exp <- checkClauses clauses sigma
        tell [DefnDecl (FunDefnok id exp)]
        typingDecls' decs

-----------------------------------------------------------
-- typing
-----------------------------------------------------------
typing :: PlatoMonad m => Program 'Untyped -> m (Program 'Typed)
typing decs = catchErrors $ updateContext (typingDecls decs)

typingExpr :: PlatoMonad m => LExpr 'Untyped -> m (LExpr 'Typed)
typingExpr exp = do
        (exp', _) <- runReaderT (inferType exp) =<< getContext =<< ask
        return exp'