module Plato.Interpreter where

import Control.Monad.Reader

import Plato.Driver.Monad
import Plato.Interpreter.Core
import Plato.Syntax.Core

evalCore :: PlatoMonad m => Term -> m ()
evalCore t = do
        ctx <- getContext =<< ask
        runReaderT (runCore t) (getCoreEnv ctx)