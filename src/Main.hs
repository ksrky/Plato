module Main where

import CLI.Constant (version)
import CLI.Options (Options (..), runWithOptions)
import Plato.Main (runPlato)

main :: IO ()
main = processOptions =<< runWithOptions

processOptions :: Options -> IO ()
processOptions (Install _) = undefined
processOptions (REPL files) = undefined
processOptions (Run src) = runPlato src
processOptions Version = putStrLn $ "Plato version " ++ version