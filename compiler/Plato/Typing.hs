{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Typing (
        typingDecls,
        typing,
        typingExpr,
) where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.IORef
import System.Log.Logger

import Control.Monad.Writer
import Plato.Common.Error
import Plato.Common.Uniq
import Plato.Driver.Logger
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
                local (extendConEnv id constrs) $
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
        liftIO $ debugM platoLog $ "Start type checking of function '" ++ show id ++ "'"
        sigma <- zonk =<< find id =<< asks getTypEnv
        exp <- checkClauses clauses sigma
        tell [DefnDecl (FunDefnok id exp)]
        typingDecls' decs

-----------------------------------------------------------
-- typing
-----------------------------------------------------------
data TypContext = TypContext
        { ctx_typenv :: TypEnv
        , ctx_conenv :: ConEnv
        , ctx_uniq :: !(IORef Uniq)
        }

initTypContext :: PlatoMonad m => m TypContext
initTypContext = do
        uref <- getUniq =<< ask
        typenv <- getTypEnv <$> (getContext =<< ask)
        conenv <- getConEnv <$> (getContext =<< ask)
        return
                TypContext
                        { ctx_typenv = typenv
                        , ctx_conenv = conenv
                        , ctx_uniq = uref
                        }

instance HasUniq TypContext where
        getUniq = return . ctx_uniq
        setUniq uniq ctx = setUniq uniq (ctx_uniq ctx)

instance HasTypEnv TypContext where
        getTypEnv = getTypEnv . ctx_typenv
        modifyTypEnv f ctx = ctx{ctx_typenv = f (ctx_typenv ctx)}

instance HasConEnv TypContext where
        getConEnv = ctx_conenv
        modifyConEnv f ctx = ctx{ctx_conenv = f (ctx_conenv ctx)}

typing :: PlatoMonad m => Program 'Untyped -> m (Program 'Typed)
typing decs = catchErrors $ do
        (decs, env) <- runReaderT (typingDecls decs) =<< initTypContext
        ctx <- getContext =<< ask
        setContext ((setTypEnv (getTypEnv env) . setConEnv (getConEnv env)) ctx) =<< ask
        return decs

typingExpr :: PlatoMonad m => LExpr 'Untyped -> m (LExpr 'Typed)
typingExpr exp = fst <$> (runReaderT (inferType exp) =<< initTypContext)