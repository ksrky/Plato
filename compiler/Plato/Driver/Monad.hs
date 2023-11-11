module Plato.Driver.Monad
    ( Plato
    , PlatoEnv (..)
    , PlatoMonad (..)
    , PlatoT
    , Session (..)
    , applyContext
    , getContext
    , initSession
    , setContext
    , unPlato
    , updateContext
    ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef

import Plato.Driver.Context
import Plato.Driver.Flag

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
    { plt_context :: !Context
    , plt_flags   :: [Flag]
    }

initPlatoEnv :: IO PlatoEnv
initPlatoEnv = do
    ctx <- initContext
    return PlatoEnv{plt_context = ctx, plt_flags = []}

----------------------------------------------------------------
-- Session
----------------------------------------------------------------
data Session = Session
    { unSession :: !(IORef PlatoEnv)
    }

initSession :: (MonadIO m) => m Session
initSession = liftIO $ Session <$> (newIORef =<< initPlatoEnv)

instance HasFlags Session where
    getFlags (Session ref) = do
        env <- liftIO $ readIORef ref
        return $ plt_flags env
    setFlag flag (Session ref) = do
        env <- liftIO $ readIORef ref
        liftIO $ writeIORef ref env{plt_flags = flag : plt_flags env}

-----------------------------------------------------------
-- Plato Monad
-----------------------------------------------------------
class (MonadReader Session m, MonadIO m, MonadCatch m) => PlatoMonad m where
    getSession :: m PlatoEnv
    setSession :: PlatoEnv -> m ()

getContext :: (PlatoMonad m) => m Context
getContext = plt_context <$> getSession

setContext :: (PlatoMonad m) => Context -> m ()
setContext ctx = do
    env <- getSession
    setSession env{plt_context = ctx}

updateContext :: (PlatoMonad m) => ReaderT Context m (a, Context) -> m a
updateContext m = do
    (res, ctx') <- runReaderT m =<< getContext
    setContext ctx'
    return res

applyContext :: (PlatoMonad m) => ReaderT Context m a -> m a
applyContext m = runReaderT m =<< getContext

type PlatoT m = ReaderT Session m

unPlato :: PlatoT m a -> Session -> m a
unPlato = runReaderT

instance (MonadIO m, MonadCatch m) => PlatoMonad (PlatoT m) where
    getSession = do
        Session ref <- ask
        liftIO $ readIORef ref
    setSession env = do
        Session ref <- ask
        liftIO $ writeIORef ref env

type Plato = PlatoT IO
