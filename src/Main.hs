module Main where

import Plato.Main (defaultStatus, repl, runPlato)
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                src : _ -> runPlato defaultStatus src
