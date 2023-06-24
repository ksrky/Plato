module Plato.Driver.Monad (
        PlatoEnv (..),
        Session (..),
        initSession,
        PlatoMonad (..),
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

import Control.Monad.RWS
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.Set qualified as S
import System.FilePath

import Control.Exception.Safe
import Plato.Common.Uniq (
        HasUniq (getUniq, setUniq),
        Uniq,
        uniqZero,
 )
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
        , plt_uniq :: !Uniq
        , plt_imported :: !Imported
        , plt_flags :: [Flag]
        }

initPlatoEnv :: PlatoEnv
initPlatoEnv =
        PlatoEnv
                { plt_entryPath = ""
                , plt_libraryPaths = []
                , plt_logPath = ""
                , plt_uniq = uniqZero
                , plt_imported = S.empty
                , plt_flags = []
                }

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data Session = Session {unSession :: !(IORef PlatoEnv)}

initSession :: MonadIO m => m Session
initSession = liftIO $ Session <$> newIORef initPlatoEnv

instance HasUniq Session where
        getUniq (Session ref) = do
                env <- liftIO $ readIORef ref
                liftIO $ newIORef $ plt_uniq env
        setUniq uniq (Session ref) = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref env{plt_uniq = uniq}

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

class (MonadReader Session m, MonadIO m, MonadCatch m) => PlatoMonad m where
        getSession :: m PlatoEnv
        setSession :: PlatoEnv -> m ()

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