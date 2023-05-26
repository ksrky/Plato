module Plato.Driver.Monad where

import Control.Monad.RWS
import Control.Monad.Reader
import Data.IORef

import Plato.Common.Error
import Plato.Common.Uniq

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
        { plt_filePath :: FilePath
        , plt_directoryPath :: FilePath
        , plt_uniq :: Uniq
        , plt_flags :: [(String, Bool)]
        }

initPlatoEnv :: PlatoEnv
initPlatoEnv =
        PlatoEnv
                { plt_filePath = ""
                , plt_directoryPath = ""
                , plt_uniq = uniqZero
                , plt_flags = [("ddump-parsing", False)]
                }

class HasInfo a where
        getFilePath :: a -> FilePath
        getDirectoryPath :: a -> FilePath

instance HasInfo PlatoEnv where
        getFilePath = plt_filePath
        getDirectoryPath = plt_directoryPath

class HasFlags a where
        getFlags :: a -> [(String, Bool)]

instance HasFlags PlatoEnv where
        getFlags = plt_flags

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

class (MonadReader Session m, MonadIO m) => PlatoMonad m where
        getSession :: m PlatoEnv
        setSession :: PlatoEnv -> m ()

setUniq :: PlatoMonad m => IORef Uniq -> m ()
setUniq ref = do
        env <- getSession
        uniq <- liftIO $ readIORef ref
        setSession env{plt_uniq = uniq}

setFlag :: PlatoMonad m => String -> Bool -> m ()
setFlag flag val = do
        env <- getSession
        setSession env{plt_flags = (flag, val) : plt_flags env}

isFlagOn :: PlatoMonad m => String -> m a -> m ()
isFlagOn flag action = do
        env <- getSession
        case lookup flag (getFlags env) of
                Just True -> void action
                Just False -> return ()
                Nothing -> unreachable $ "flag not found '" ++ flag ++ "'"

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