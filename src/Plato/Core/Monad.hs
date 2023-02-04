module Plato.Core.Monad where

import Control.Monad.Reader
import Data.IORef

import Plato.Syntax.Core

data CoreEnv = CoreEnv
        { core_unique :: IORef Id
        , core_context :: Context
        }

type Core m a = ReaderT CoreEnv m a

extendContext :: Monad m => (Context -> Context) -> Core m a -> Core m a
extendContext f = local (\env -> env{core_context = f (core_context env)})

newId :: MonadIO m => Core m Id
newId = do
        ref <- asks core_unique
        Id u <- liftIO $ readIORef ref
        liftIO $ writeIORef ref (Id (u + 1))
        return (Id u)

new :: [Word]
new = [1 :: Word ..]