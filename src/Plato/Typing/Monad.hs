{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Typing.Monad where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env

data Context = Context
        { typenv :: TypEnv
        , uniq :: IORef Uniq
        , errloc :: IORef Span
        }

instance HasTypEnv Context where
        getEnv = getEnv . typenv
        modifyEnv f ctx = ctx{typenv = f (typenv ctx)}

-- Creating, reading and writing IORef
newMIORef :: MonadIO m => a -> m (IORef a)
newMIORef = liftIO . newIORef

readMIORef :: MonadIO m => IORef a -> m a
readMIORef = liftIO . readIORef

writeMIORef :: MonadIO m => IORef a -> a -> m ()
writeMIORef = (liftIO .) . writeIORef

-- | Creating and rewriting Uniq
newUniq :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Uniq
newUniq = pickUniq =<< ask

{- | Variable generation
newVarIdent :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Ident
newVarIdent = freshIdent dummyVN
-}

-- | Type variable generation
newTyVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => TyVar -> m TyVar
newSkolemTyVar tv = SkolemTv (unTyVar tv) <$> newUniq

newMetaTv :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m MetaTv
newMetaTv = MetaTv <$> newUniq <*> liftIO (newIORef Nothing)

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Tau)
readMetaTv (MetaTv _ ref) = liftIO $ readIORef ref

writeMetaTv :: MonadIO m => MetaTv -> Tau -> m ()
writeMetaTv (MetaTv _ ref) ty = liftIO $ writeIORef ref (Just ty)

-- | Kind variable generation
newKnVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m MetaKv
newMetaKv = MetaKv <$> newUniq <*> liftIO (newIORef Nothing)

readMetaKv :: MonadIO m => MetaKv -> m (Maybe Kind)
readMetaKv (MetaKv _ ref) = liftIO $ readIORef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> m ()
writeMetaKv (MetaKv _ ref) ty = liftIO $ writeIORef ref (Just ty)

-- | Context management
initContext :: MonadIO m => TypEnv -> m Context
initContext typenv = do
        uniq <- liftIO $ newIORef 0
        errloc <- liftIO $ newIORef NoSpan
        return $ Context typenv uniq errloc