module Plato.Common.Uniq where

import Control.Monad.IO.Class
import Data.IORef
import Data.Text              qualified as T

import Plato.Common.Pretty

-- | Uniq
newtype Uniq
    = Uniq Word
    deriving (Eq, Num, Ord)

uniq2text :: Uniq -> T.Text
uniq2text (Uniq w) = T.pack $ "$" ++ show w

instance Show Uniq where
    show (Uniq w) = show w

instance Pretty Uniq where
    pretty (Uniq w) = viaShow w

class HasUniq a where
    getUniq :: (MonadIO m) => a -> m (IORef Uniq)
    setUniq :: (MonadIO m) => Uniq -> a -> m ()
    readUniq :: (MonadIO m) => a -> m Uniq
    readUniq env = liftIO $ readIORef =<< getUniq env
    pickUniq :: (MonadIO m) => a -> m Uniq
    pickUniq env = do
        ref <- getUniq env
        u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (u + 1)
        return u

instance HasUniq (IORef Uniq) where
    getUniq = return
    setUniq uniq ref = liftIO $ writeIORef ref uniq

instance (HasUniq a) => HasUniq (a, b) where
    getUniq = getUniq . fst
    setUniq uniq = setUniq uniq . fst

uniqZero :: Uniq
uniqZero = 0

initUniq :: (MonadIO m) => m (IORef Uniq)
initUniq = liftIO $ newIORef uniqZero
