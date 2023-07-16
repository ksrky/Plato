module Plato.Typing.Utils (
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
        getConstrs,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask), asks)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Typing.Env

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
newVarIdent = freshIdent $ genName "$"

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

getConstrs :: (MonadReader env m, HasConEnv env, MonadThrow m) => Type -> m Constrs
getConstrs ty = do
        let getTycon :: Type -> Ident
            getTycon (AllT _ ty) = getTycon (unLoc ty)
            getTycon (ArrT _ res) = getTycon (unLoc res)
            getTycon (AppT fun _) = getTycon (unLoc fun)
            getTycon (ConT tc) = tc
            getTycon _ = unreachable "Not a variant type"
        lookupIdent (getTycon ty) =<< asks getConEnv