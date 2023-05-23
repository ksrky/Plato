module Plato.Driver.Monad where

import Control.Monad.RWS
import Control.Monad.Reader
import Data.IORef

import Plato.Common.Debug
import Plato.Common.Uniq

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
        { plt_filePath :: FilePath
        , plt_directoryPath :: FilePath
        , plt_uniq :: Uniq
        }

instance HasInfo PlatoEnv where
        getFilePath = plt_filePath
        getDirectoryPath = plt_directoryPath

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data Session = Session {unSession :: !(IORef PlatoEnv)}

instance HasUniq Session where
        getUniq (Session ref) = do
                ref <- liftIO $ readIORef ref
                liftIO $ newIORef $ plt_uniq ref

class (MonadReader Session m, MonadIO m) => PlatoMonad m where
        getSession :: m PlatoEnv
        setSession :: PlatoEnv -> m ()

setUniq :: PlatoMonad m => IORef Uniq -> m ()
setUniq ref = do
        env <- getSession
        uniq <- liftIO $ readIORef ref
        setSession env{plt_uniq = uniq}

---------------------------------------------------------------
-- Plato
----------------------------------------------------------------
type Plato = ReaderT Session IO

unPlato :: Plato a -> Session -> IO a
unPlato = runReaderT

instance PlatoMonad Plato where
        getSession = do
                Session ref <- ask
                liftIO $ readIORef ref
        setSession env = do
                Session ref <- ask
                liftIO $ writeIORef ref env