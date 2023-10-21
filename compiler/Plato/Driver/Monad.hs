module Plato.Driver.Monad (
        PlatoEnv (..),
        Session (..),
        initSession,
        getContext,
        setContext,
        PlatoMonad (..),
        updateContext,
        runContext,
        PlatoT,
        unPlato,
        Plato,
        -- Plato.Driver.Flag
        HasFlags (..),
        Flag (..),
        whenFlagOn,
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
        , plt_flags :: [Flag]
        }

initPlatoEnv :: IO PlatoEnv
initPlatoEnv = do
        ctx <- initContext
        return
                PlatoEnv
                        { plt_context = ctx
                        , plt_flags = []
                        }

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data Session = Session {unSession :: !(IORef PlatoEnv)}

initSession :: (MonadIO m) => m Session
initSession = liftIO $ Session <$> (newIORef =<< initPlatoEnv)

getContext :: (MonadIO m) => Session -> m Context
getContext (Session ref) = liftIO $ plt_context <$> readIORef ref

setContext :: (MonadIO m) => Context -> Session -> m ()
setContext ctx (Session ref) = do
        env <- liftIO $ readIORef ref
        liftIO $ writeIORef ref env{plt_context = ctx}

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

type PlatoT m = ReaderT Session m

unPlato :: PlatoT m a -> Session -> m a
unPlato = runReaderT

updateContext :: (PlatoMonad m) => ReaderT Context m (a, Context) -> m a
updateContext m = do
        ctx <- getContext =<< ask
        (res, ctx') <- runReaderT m ctx
        setContext ctx' =<< ask
        return res

runContext :: (PlatoMonad m) => ReaderT Context m a -> m a
runContext m = do
        ctx <- getContext =<< ask
        runReaderT m ctx

instance (MonadIO m, MonadCatch m) => PlatoMonad (PlatoT m) where
        getSession = do
                Session ref <- ask
                liftIO $ readIORef ref
        setSession env = do
                Session ref <- ask
                liftIO $ writeIORef ref env

type Plato = PlatoT IO