module Main where

import Control.Monad

import Depends
import Options
import Plato
import REPL

main :: IO ()
main = processCommands =<< runWithCommand

processCommands :: Command -> IO ()
processCommands (REPL files opts) = do
        session <- initSession
        setInfo "interactive" (searchPaths opts) (logPath opts) session
        processOptions opts session
        setFlag FEvalCore session
        libfiles <- concat <$> mapM readDepend (searchPaths opts)
        repl (libfiles ++ files) session
processCommands (Run files opts) = do
        session <- initSession
        libfiles <- concat <$> mapM readDepend (searchPaths opts)
        forM_ (libfiles ++ files) $ \file -> do
                setInfo file (searchPaths opts) (logPath opts) session
                processOptions opts session
                runPlato file session
processCommands (Version version) = putStrLn $ "Plato version " ++ version

processOptions :: Options -> Session -> IO ()
processOptions opts session = do
        when (isDebug opts) $ setFlag FDebug session
        when (printParsed opts) $ setFlag FPrintParsed session
        when (printTyped opts) $ setFlag FPrintTyped session
        when (printCore opts) $ setFlag FPrintCore session
        initLogger session