module Plato.Driver.Logger (
        platoLog,
        initLogger,
) where

import Control.Monad.IO.Class
import System.FilePath
import System.IO
import System.Log
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

import Plato.Driver.Info

getDebugLogPath :: (HasInfo env, MonadIO m) => env -> m FilePath
getDebugLogPath env = do
        path <- getEntryPath env
        return $ replaceExtension path ".debug.log"

getUserLogPath :: (HasInfo env, MonadIO m) => env -> m FilePath
getUserLogPath env = do
        path <- getLogPath env
        return $ replaceExtension path ".log"

platoLog :: String
platoLog = "PlatoLog"

initLogger :: (HasInfo env, MonadIO m) => env -> m ()
initLogger env = do
        debugLogPath <- getDebugLogPath env
        userLogPath <- getUserLogPath env
        debugLogHandler <- liftIO $ withFormatter <$> fileHandler debugLogPath DEBUG
        userLogHandler <- liftIO $ withFormatter <$> fileHandler userLogPath WARNING
        errorStreamHandler <- liftIO $ streamHandler stderr ERROR
        liftIO $
                updateGlobalLogger
                        platoLog
                        (setLevel DEBUG . setHandlers [debugLogHandler, userLogHandler, errorStreamHandler])

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where
        formatter = simpleLogFormatter "[$loggername $prio] $msg"