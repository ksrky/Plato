module Plato.Interpreter where

import Control.Monad.Reader

import Plato.Common.Error
import Plato.Driver.Monad
import Plato.Interpreter.Core
import Plato.Syntax.Core
import Plato.Core.Closure

addCoreEnv :: PlatoMonad m => Prog -> m ()
addCoreEnv prog = do
        ctx <- getContext =<< ask
        sc <- runReaderT (enterCore prog) (getCoreEnv ctx)
        let CoreEnv env _ = getCoreEnv ctx
        setContext (setCoreEnv (CoreEnv env sc) ctx) =<< ask

evalCore :: PlatoMonad m => Term -> m ()
evalCore t = catchErrors $ do
        ctx <- getContext =<< ask
        sc <- ask getScope
        runReaderT (runCore sc t) (getCoreEnv ctx)