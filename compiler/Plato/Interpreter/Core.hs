module Plato.Interpreter.Core where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef

import Control.Exception.Safe
import Plato.Core.Data
import Plato.Core.Eval
import Plato.Core.Scope
import Plato.Syntax.Core

data CoreEnv = CoreEnv (IORef EnvEntries) Scope

initCoreEnv :: IO CoreEnv
initCoreEnv = do
        eref <- newIORef []
        return $ CoreEnv eref emptyScope

class HasCoreEnv e where
        getCoreEnv :: e -> CoreEnv
        modifyCoreEnv :: (CoreEnv -> CoreEnv) -> e -> e
        setCoreEnv :: CoreEnv -> e -> e
        setCoreEnv = modifyCoreEnv . const

runCore :: (MonadReader e m, HasCoreEnv e, MonadThrow m, MonadIO m) => Term -> m ()
runCore t = do
        CoreEnv env sc <- asks getCoreEnv
        val <- runReaderT (eval (t, sc)) env
        liftIO $ print val
