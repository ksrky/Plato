module Plato.Driver.Monad (
        PlatoEnv (..),
        Session (..),
        initSession,
        PlatoMonad (..),
        PlatoT,
        unPlato,
        Plato,
        -- PLato.Driver.Info
        HasInfo (..),
) where

import Control.Monad.RWS
import Control.Monad.Reader
import Data.IORef
import Data.Set qualified as S

import Plato.Common.Uniq (
        HasUniq (getUniq, setUniq),
        Uniq,
        uniqZero,
 )
import Plato.Driver.Import
import Plato.Driver.Info
import System.FilePath

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
        { plt_entryPath :: !FilePath
        , plt_directoryPath :: !FilePath
        , plt_libraryPaths :: ![FilePath]
        , plt_uniq :: !Uniq
        , plt_imported :: !Imported
        , plt_flags :: [(String, Bool)]
        }

initPlatoEnv :: PlatoEnv
initPlatoEnv =
        PlatoEnv
                { plt_entryPath = ""
                , plt_directoryPath = ""
                , plt_libraryPaths = []
                , plt_uniq = uniqZero
                , plt_imported = S.empty
                , plt_flags = [("ddump-parsing", False), ("ddump-core", False)]
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
        getDirectoryPath (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_directoryPath env
        getLibraryPaths (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_libraryPaths env
        setInfo path libs (Session ref) = do
                env <- liftIO $ readIORef ref
                let env' =
                        env
                                { plt_entryPath = path
                                , plt_directoryPath = takeDirectory path
                                , plt_libraryPaths = libs
                                }
                liftIO $ writeIORef ref env'

instance HasImported Session where
        getImported (Session ref) = do
                env <- liftIO $ readIORef ref
                return $ plt_imported env
        setImported impedSet (Session ref) = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref env{plt_imported = impedSet}

class (MonadReader Session m, MonadIO m) => PlatoMonad m where
        getSession :: m PlatoEnv
        setSession :: PlatoEnv -> m ()

---------------------------------------------------------------
-- Plato
----------------------------------------------------------------

type PlatoT m = ReaderT Session m

unPlato :: PlatoT m a -> Session -> m a
unPlato = runReaderT

instance MonadIO m => PlatoMonad (PlatoT m) where
        getSession = do
                Session ref <- ask
                liftIO $ readIORef ref
        setSession env = do
                Session ref <- ask
                liftIO $ writeIORef ref env

type Plato = PlatoT IO