module Plato.Syntax.Typing.Helper where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing

-- Creating, reading and writing IORef
newMIORef :: MonadIO m => a -> m (IORef a)
newMIORef = liftIO . newIORef

readMIORef :: MonadIO m => IORef a -> m a
readMIORef = liftIO . readIORef

writeMIORef :: MonadIO m => IORef a -> a -> m ()
writeMIORef = (liftIO .) . writeIORef

-- | Creating and rewriting Uniq
newUniq :: (MonadReader e m, HasUniq e, MonadIO m) => m Uniq
newUniq = pickUniq =<< ask

-- | Variable generation
newVarIdent :: (MonadReader e m, HasUniq e, MonadIO m) => m Ident
newVarIdent = freshIdent dummyName

-- | Type variable generation
newTyVar :: (MonadReader e m, HasUniq e, MonadIO m) => m Type
newTyVar = MetaT <$> newMetaTv

newSkolemTv :: (MonadReader e m, HasUniq e, MonadIO m) => TyVar -> m TyVar
newSkolemTv tv = SkolemTv <$> reassignUniq (unTyVar tv)

newMetaTv :: (MonadReader e m, HasUniq e, MonadIO m) => m MetaTv
newMetaTv = MetaTv <$> newUniq <*> newMIORef Nothing

readMetaTv :: MonadIO m => MetaTv -> m (Maybe Type)
readMetaTv (MetaTv _ ref) = readMIORef ref

writeMetaTv :: MonadIO m => MetaTv -> Type -> m ()
writeMetaTv (MetaTv _ ref) ty = writeMIORef ref (Just ty)

-- | Kind variable generation
newKnVar :: (MonadReader e m, HasUniq e, MonadIO m) => m Kind
newKnVar = MetaK <$> newMetaKv

newMetaKv :: (MonadReader e m, HasUniq e, MonadIO m) => m MetaKv
newMetaKv = MetaKv <$> newUniq <*> newMIORef Nothing

readMetaKv :: MonadIO m => MetaKv -> m (Maybe Kind)
readMetaKv (MetaKv _ ref) = readMIORef ref

writeMetaKv :: MonadIO m => MetaKv -> Kind -> m ()
writeMetaKv (MetaKv _ ref) ty = writeMIORef ref (Just ty)

splitConstrTy :: Rho -> ([Sigma], Tau)
splitConstrTy = go []
    where
        go :: [Sigma] -> Rho -> ([Sigma], Tau)
        go acc (ArrT sigma rho) = go (unLoc sigma : acc) (unLoc rho)
        go acc tau = (reverse acc, tau)