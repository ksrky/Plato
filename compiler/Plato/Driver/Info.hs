module Plato.Driver.Info where

import Control.Monad.IO.Class

class HasInfo a where
        getEntryPath :: MonadIO m => a -> m FilePath
        getDirectoryPath :: MonadIO m => a -> m FilePath
        setInfo :: MonadIO m => FilePath -> a -> m ()
