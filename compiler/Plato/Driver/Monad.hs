module Plato.Driver.Monad where

import Control.Monad.RWS
import Control.Monad.Reader
import Data.IORef
import Data.Set qualified as S

import Plato.Common.Error
import Plato.Common.Uniq
import Plato.Driver.Import
import Plato.Driver.Info
import System.FilePath

----------------------------------------------------------------
-- Plato Env
----------------------------------------------------------------
data PlatoEnv = PlatoEnv
        { plt_entryPath :: !FilePath
        , plt_directoryPath :: !FilePath
        , plt_uniq :: !Uniq
        , plt_imported :: !Imported
        , plt_flags :: [(String, Bool)]
        }

initPlatoEnv :: PlatoEnv
initPlatoEnv =
        PlatoEnv
                { plt_entryPath = ""
                , plt_directoryPath = ""
                , plt_uniq = uniqZero
                , plt_imported = S.empty
                , plt_flags = [("ddump-parsing", False), ("ddump-core", False)]
                }

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
        setInfo path (Session ref) = do
                env <- liftIO $ readIORef ref
                let env' = env{plt_entryPath = path, plt_directoryPath = takeDirectory path}
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

{-setUniq :: PlatoMonad m => IORef Uniq -> m ()
setUniq ref = do
        env <- getSession
        uniq <- liftIO $ readIORef ref
        setSession env{plt_uniq = uniq-}

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