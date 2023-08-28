module Plato.Driver.Monad (
        PlatoEnv (..),
        Session (..),
        initSession,
        getContext,
        setContext,
        PlatoMonad (..),
        updateContext,
        PlatoT,
        unPlato,
        Plato,
        getFileName,
        -- PLato.Driver.Info
        HasInfo (..),
        -- Plato.Driver.Logger
        initLogger,
        -- Plato.Driver.Flag
        HasFlags (..),
        Flag (..),
        whenFlagOn,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Set qualified as S
import System.FilePath

import Plato.Common.Uniq
import Plato.Driver.Context
import Plato.Driver.Flag
import Plato.Driver.Import
import Plato.Driver.Info
import Plato.Driver.Logger

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
        { plt_entryPath :: !FilePath
        , plt_libraryPaths :: ![FilePath]
        , plt_logPath :: !FilePath
        , plt_context :: !Context
        , plt_imported :: !Imported
        , plt_flags :: [Flag]
        }

initPlatoEnv :: IO PlatoEnv
initPlatoEnv = do
        ctx <- initContext
        return
                PlatoEnv
                        { plt_entryPath = ""
                        , plt_libraryPaths = []
                        , plt_logPath = ""
                        , plt_context = ctx
                        , plt_imported = S.empty
                        , plt_flags = []
                        }

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data Session = Session {unSession :: !(IORef PlatoEnv)}

initSession :: MonadIO m => m Session
initSession = liftIO $ Session <$> (newIORef =<< initPlatoEnv)

getContext :: MonadIO m => Session -> m Context
getContext (Session ref) = liftIO $ plt_context <$> readIORef ref

setContext :: MonadIO m => Context -> Session -> m ()
setContext ctx (Session ref) = do
        env <- liftIO $ readIORef ref
        liftIO $ writeIORef ref env{plt_context = ctx}

instance HasUniq Session where
        getUniq session = ctx_uniq <$> getContext session
        setUniq uniq session = do
                ctx <- getContext session
                liftIO $ writeIORef (ctx_uniq ctx) uniq

instance HasInfo Session where
        getEntryPath (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_entryPath env
        getCurrentDirPath session = takeDirectory <$> getEntryPath session
        getLibraryPaths (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_libraryPaths env
        getLogPath (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_logPath env
        setInfo entryPath libPaths logPath (Session ref) = do
                env <- liftIO $ readIORef ref
                let env' =
                        env
                                { plt_entryPath = entryPath
                                , plt_libraryPaths = libPaths
                                , plt_logPath = fromMaybe entryPath logPath
                                }
                liftIO $ writeIORef ref env'

instance HasImported Session where
        getImported (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_imported env
        setImported impedSet (Session ref) = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref env{plt_imported = impedSet}

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

updateContext :: PlatoMonad m => ReaderT Context m (a, Context) -> m a
updateContext m = do
        ctx <- getContext =<< ask
        (res, ctx') <- runReaderT m ctx
        setContext ctx' =<< ask
        return res

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

---------------------------------------------------------------
-- Utilities
----------------------------------------------------------------
getFileName :: PlatoMonad m => m String
getFileName = takeBaseName <$> (getEntryPath =<< ask)