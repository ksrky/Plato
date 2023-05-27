module Plato.Typing.Monad (
        HasTypEnv (..),
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
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env

data Context = Context
        { typenv :: TypEnv
        , uniq :: IORef Uniq
        , conenv :: ConEnv
        }

initContext :: (MonadReader env m, HasUniq env, MonadIO m) => m Context
initContext = do
        uniq <- getUniq =<< ask
        return Context{typenv = initTypEnv, uniq = uniq, conenv = initConEnv}

instance HasUniq Context where
        getUniq = return . uniq

instance HasTypEnv Context where
        getEnv = getEnv . typenv
        modifyEnv f ctx = ctx{typenv = f (typenv ctx)}

instance HasConEnv Context where
        getConEnv = getConEnv . conenv
        modifyConEnv f ctx = ctx{conenv = f (conenv ctx)}

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