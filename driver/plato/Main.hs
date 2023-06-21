module Main where

import Options
import Plato

main :: IO ()
main = processCommands =<< runWithCommand

processCommands :: Command -> IO ()
processCommands (REPL _files _opts) = error "not implemented"
processCommands (Run src opts) = do
        session <- initSession
        setInfo src (libraryPaths opts) (logPath opts) session
        initLogger session
        runPlato src session
processCommands (Version version) = putStrLn $ "Plato version " ++ version