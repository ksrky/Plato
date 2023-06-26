module Main where

import Control.Monad

import Options
import Plato
import REPL

main :: IO ()
main = processCommands =<< runWithCommand

processCommands :: Command -> IO ()
processCommands (REPL files opts) = do
        session <- initSession
        setInfo "interactive" (libraryPaths opts) (logPath opts) session
        processOptions opts session
        setFlag FEvalCore session
        repl files session
processCommands (Run src opts) = do
        session <- initSession
        setInfo src (libraryPaths opts) (logPath opts) session
        processOptions opts session
        runPlato src session
processCommands (Version version) = putStrLn $ "Plato version " ++ version

processOptions :: Options -> Session -> IO ()
processOptions opts session = do
        when (isDebug opts) $ setFlag FDebug session
        initLogger session