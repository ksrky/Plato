module Plato.Core.Monad where

import Control.Monad.Reader
import Data.IORef

import Plato.Syntax.Core

data CoreEnv = CoreEnv
        { core_unique :: IORef Unique
        , core_context :: Context
        }

type Core m a = ReaderT CoreEnv m a

extendContext :: Monad m => (Context -> Context) -> Core m a -> Core m a
extendContext f = local (\env -> env{core_context = f (core_context env)})

newUnique :: MonadIO m => Core m Unique
newUnique = do
        ref <- asks core_unique
        Unique u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (Unique (u + 1))
        return (Unique u)