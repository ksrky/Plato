module Plato.Driver.Logger where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Plato.Driver.Monad
import System.FilePath
import System.IO
import System.Log
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

debugLogPath :: PlatoMonad m => m FilePath
debugLogPath = do
        path <- getEntryPath =<< ask
        return $ replaceExtension path ".debug.log"

userLogPath :: PlatoMonad m => m FilePath
userLogPath = do
        path <- getLogPath =<< ask
        return $ replaceExtension path ".log"

platoLog :: String
platoLog = "PlatoLog"

initLogger :: PlatoMonad m => m ()
initLogger = do
        logPath <- getLogPath =<< ask
        debugLogHandler <- liftIO $ withFormatter <$> fileHandler logPath DEBUG
        userLogHandler <- liftIO $ withFormatter <$> fileHandler logPath WARNING
        liftIO $
                updateGlobalLogger
                        platoLog
                        (setLevel DEBUG . setHandlers [debugLogHandler, userLogHandler])

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where
        formatter = simpleLogFormatter "[$time $loggername $prio] $msg"