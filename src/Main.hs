module Main where

import qualified Plato.Main
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> Plato.Main.repl
                src : _ -> Plato.Main.processFile src
