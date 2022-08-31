module Main where

import Plato.Main
import System.Environment (getArgs)

version :: String
version = "0.1.0.0"

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                "--version" : _ -> putStrLn $ "Plato version " ++ version
                src : opts -> case opts of
                        "--debug" : _ -> debugPlato defaultStatus{debug = True, source = src} src
                        _ -> runPlato defaultStatus{source = src} src
