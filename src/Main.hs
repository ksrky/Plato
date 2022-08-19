module Main where

import Plato.Main (repl, runPlato)
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                src : _ -> runPlato src
