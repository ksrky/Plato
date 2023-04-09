{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Typing.Monad where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.IORef

import Plato.Common.Global
import Plato.Common.Location
import Plato.Syntax.Typing.Ident as Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type
import Plato.Typing.Env

data Context = Context
        { typenv :: Env
        , uniq :: IORef Unique
        , errloc :: IORef Span
        }

newtype Typ m a = Typ {runTyp :: Context -> m a}

instance Monad m => Functor (Typ m) where
        fmap f (Typ m) = Typ $ \ctx -> do
                x <- m ctx
                return (f x)

instance Monad m => Applicative (Typ m) where
        pure x = Typ (\_ -> return x)
        f <*> x = Typ $ \ctx -> do
                f' <- runTyp f ctx
                runTyp (fmap f' x) ctx

instance Monad m => Monad (Typ m) where
        m >>= k = Typ $ \ctx -> do
                v <- runTyp m ctx
                runTyp (k v) ctx

instance MonadThrow m => MonadThrow (Typ m)

instance MonadTrans Typ where
        lift m = Typ (const m)

instance Monad m => MonadReader Env (Typ m) where
        ask = Typ (return . typenv)
        local f (Typ m) = Typ (\ctx -> m ctx{typenv = f (typenv ctx)})

asksM :: (Env -> m a) -> Typ m a
asksM f = Typ (f . typenv)

-- | creating, reading and writing IORef
newTypRef :: MonadIO m => a -> Typ m (IORef a)
newTypRef v = lift (liftIO $ newIORef v)

readTypRef :: MonadIO m => IORef a -> Typ m a
readTypRef r = lift (liftIO $ readIORef r)

writeTypRef :: MonadIO m => IORef a -> a -> Typ m ()
writeTypRef r v = lift (liftIO $ writeIORef r v)

-- creating and rewriting Uniq
newUniq :: MonadIO m => Typ m Unique
newUniq = Typ $ \ctx -> do
        let ref = uniq ctx
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

-- | type variable generation
newTyVar :: MonadIO m => Typ m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: MonadIO m => TyVar -> Typ m TyVar
newSkolemTyVar tv = do
        u <- newUniq
        return $ SkolemTv (unTyVar tv){stamp = u}

newMetaTv :: MonadIO m => Typ m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newTypRef Nothing

readMetaTv :: MonadIO m => MetaTv -> Typ m (Maybe Tau)
readMetaTv (MetaTv _ ref) = readTypRef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> Typ m ()
writeMetaTv (MetaTv _ ref) ty = writeTypRef ref (Just ty)

-- | kind variable generation
newKnVar :: MonadIO m => Typ m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: MonadIO m => Typ m MetaKv
newMetaKv = MetaKv <$> newUniq <*> newTypRef Nothing

readMetaKv :: MonadIO m => MetaKv -> Typ m (Maybe Kind)
readMetaKv (MetaKv _ ref) = readTypRef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> Typ m ()
writeMetaKv (MetaKv _ ref) ty = writeTypRef ref (Just ty)

-- | writeing error location
readErrLoc :: MonadIO m => Typ m Span
readErrLoc = Typ $ \env -> do
        let ref = errloc env
        liftIO $ readIORef ref

writeErrLoc :: MonadIO m => Span -> Typ m ()
writeErrLoc sp = Typ $ \env -> do
        let ref = errloc env
        liftIO $ writeIORef ref sp

-- | Context management
initContext :: MonadIO m => Env -> m Context
initContext typenv = do
        uniq <- liftIO $ newIORef 0
        errloc <- liftIO $ newIORef NoSpan
        return $ Context typenv uniq errloc