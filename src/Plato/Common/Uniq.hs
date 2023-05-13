{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Plato.Common.Uniq where

import Control.Monad.IO.Class
import Data.IORef

newtype Uniq = Uniq Word
        deriving (Eq, Show, Ord, Num)

class HasUniq a where
        getUniq :: MonadIO m => a -> m (IORef Uniq)
        pickUniq :: MonadIO m => a -> m Uniq
        pickUniq env = do
                ref <- getUniq env
                u <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (u + 1)
                return u

instance HasUniq (IORef Uniq) where
        getUniq = return
