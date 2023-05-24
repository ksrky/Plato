module Plato.Common.Uniq (
        Uniq,
        uniq2text,
        HasUniq (..),
        uniqZero,
        initUniq,
) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Text qualified as T
import Prettyprinter

-- | Uniq
newtype Uniq = Uniq Word
        deriving (Eq, Show, Ord, Num)

uniq2text :: Uniq -> T.Text
uniq2text (Uniq w) = T.pack $ "$" ++ show w

instance Pretty Uniq where
        pretty (Uniq w) = "$" <> viaShow w

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

uniqZero :: Uniq
uniqZero = 0

initUniq :: MonadIO m => m (IORef Uniq)
initUniq = liftIO $ newIORef uniqZero