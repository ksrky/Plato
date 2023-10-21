module Plato.Driver.Interactive (
        Interactive,
        getEnvEntries,
        getCoreScope,
        runInteractive,
        evalCore,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Prettyprinter.Render.Text

import Plato.Common.Uniq
import Plato.Core.Closure
import Plato.Core.Env
import Plato.Core.Eval
import Plato.Core.Pretty
import Plato.Driver.Monad
import Plato.Syntax.Core

data InteractEnv = InteractEnv
        { int_envent :: IORef EnvEntries
        , int_scope :: Scope
        , int_uniq :: IORef Uniq
        }

type Interactive m = ReaderT InteractEnv m

getEnvEntries :: (MonadIO m) => Interactive m (IORef EnvEntries)
getEnvEntries = asks int_envent

getCoreScope :: (Monad m) => Interactive m Scope
getCoreScope = asks int_scope

instance HasUniq InteractEnv where
        getUniq = return . int_uniq
        setUniq uniq = setUniq uniq . int_uniq

instance HasCoreEnv InteractEnv where
        getCoreEnv = getCoreEnv . int_envent
        setCoreEnv = setCoreEnv . int_envent

runInteractive ::
        (PlatoMonad m) =>
        Interactive m a ->
        m a
runInteractive m = do
        eref <- getCoreEnv =<< getContext =<< ask
        uref <- getUniq =<< getContext =<< ask
        env <- liftIO $ readIORef eref
        runReaderT m
                $ InteractEnv
                        { int_envent = eref
                        , int_scope = restoreScope env
                        , int_uniq = uref
                        }

evalCore :: (MonadThrow m, MonadIO m) => Term -> Interactive m ()
evalCore t = do
        sc <- getCoreScope
        liftIO . putDoc =<< evalPrint =<< eval (t, sc)
        liftIO $ putStrLn ""