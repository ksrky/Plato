module Plato.PsToTyp.Error where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location

data ScopeError = ScopeError Ident

instance Show ScopeError where
        show (ScopeError id) =
                locatedErrorMessage (getLoc id) $ hsep ["Not in scope", squotes $ pretty id]

instance Exception ScopeError

throwScopeError :: MonadThrow m => Ident -> m a
throwScopeError id = throw $ ScopeError id

catchScopeError :: (MonadCatch m, MonadIO m, Monoid a) => m a -> m a
catchScopeError = (`catch` (\e@ScopeError{} -> liftIO (print e) >> return mempty))