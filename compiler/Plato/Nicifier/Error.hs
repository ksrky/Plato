module Plato.Nicifier.Error where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Location

data FixResolError = FixResolError Span String

instance Show FixResolError where
        show (FixResolError sp msg) = locatedErrorMessage sp $ pretty msg

instance Exception FixResolError

throwFixResolError :: MonadThrow m => Span -> String -> m a
throwFixResolError sp msg = throw $ FixResolError sp msg

catchFixResolError :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchFixResolError = (`catch` (\e@FixResolError{} -> liftIO (print e) >> return mempty))