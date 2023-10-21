module Plato.Driver.Flag where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.IORef

data Flag = FDebug | FPrintParsed | FPrintTyped | FPrintCore
        deriving (Eq, Show)

class HasFlags a where
        getFlags :: (MonadIO m) => a -> m [Flag]
        setFlag :: (MonadIO m) => Flag -> a -> m ()
        isFlagOn :: (MonadIO m) => Flag -> a -> m Bool
        isFlagOn flag env = do
                flags <- getFlags env
                return $ flag `elem` flags

instance HasFlags (IORef [Flag]) where
        getFlags = liftIO . readIORef
        setFlag flag ref = do
                flags <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (flag : flags)

whenFlagOn :: (MonadReader e m, HasFlags e, MonadIO m) => Flag -> m a -> m ()
whenFlagOn flag action = do
        flagOn <- isFlagOn flag =<< ask
        when flagOn $ void action