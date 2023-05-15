{-# LANGUAGE FlexibleInstances #-}

module Plato.Typing.Monad (
        Context (Context),
        HasEnv (..),
        newMIORef,
        readMIORef,
        writeMIORef,
        newUniq,
        newVarIdent,
        newTyVar,
        newSkolemTyVar,
        newMetaTv,
        readMetaTv,
        writeMetaTv,
        newKnVar,
        newMetaKv,
        readMetaKv,
        writeMetaKv,
        initContext,
        returnContext,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Control.Monad.State.Class
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env

data Context = Context
        { typenv :: TypEnv
        , uniq :: IORef Uniq
        , errloc :: IORef Span
        }

instance HasUniq Context where
        getUniq = return . uniq

class HasEnv a where
        getEnv :: Monad m => a -> m TypEnv
        modifyEnv :: (TypEnv -> TypEnv) -> a -> a

instance HasEnv TypEnv where
        getEnv = return
        modifyEnv = id

instance HasEnv Context where
        getEnv = getEnv . typenv
        modifyEnv f ctx = ctx{typenv = f (typenv ctx)}

-- Creating, reading and writing IORef
newMIORef :: MonadIO m => a -> m (IORef a)
newMIORef = newMIORef

readMIORef :: MonadIO m => IORef a -> m a
readMIORef = liftIO . readIORef

writeMIORef :: MonadIO m => IORef a -> a -> m ()
writeMIORef = (liftIO .) . writeIORef

-- | Creating and rewriting Uniq
newUniq :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Uniq
newUniq = pickUniq =<< ask

-- | Variable generation
newVarIdent :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Ident
newVarIdent = freshIdent dummyVN

-- | Type variable generation
newTyVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTyVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => TyVar -> m TyVar
newSkolemTyVar tv = do
        u <- newUniq
        return $ SkolemTv (unTyVar tv){stamp = u}

newMetaTv :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newMIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ ref) = readMIORef ref

writeMetaTv :: MonadIO m => MetaTv -> Type -> m ()
writeMetaTv (MetaTv _ ref) ty = writeMIORef ref (Just ty)

-- | Kind variable generation
newKnVar :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => m MetaKv
newMetaKv = MetaKv <$> newUniq <*> newMIORef Nothing

readMetaKv :: MonadIO m => MetaKv -> m (Maybe Kind)
readMetaKv (MetaKv _ ref) = readMIORef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> m ()
writeMetaKv (MetaKv _ ref) ty = writeMIORef ref (Just ty)

-- | Context management
initContext :: (MonadState s m, HasEnv s, MonadReader r m, HasUniq r, MonadIO m) => m Context
initContext = do
        typenv <- getEnv =<< get
        uniq <- getUniq =<< ask
        errloc <- liftIO $ newIORef NoSpan
        return $ Context typenv uniq errloc

returnContext :: (MonadState env m, HasEnv env) => Context -> m ()
returnContext = modify . modifyEnv . const . typenv