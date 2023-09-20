module Machine.Utils where

import Control.Monad.IO.Class
import Data.IORef

newMIORef :: MonadIO m => a -> m (IORef a)
newMIORef = liftIO . newIORef

readMIORef :: MonadIO m => IORef a -> m a
readMIORef = liftIO . readIORef

writeMIORef :: MonadIO m => IORef a -> a -> m ()
writeMIORef ref = liftIO . writeIORef ref

modifyMIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyMIORef ref = liftIO . modifyIORef ref
