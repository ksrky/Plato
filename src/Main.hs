module Main where

import Iface.Constant (version)
import Iface.Options (Options (..), runWithOptions)
import Plato.Main (repl, runPlato)

main :: IO ()
main = processOptions =<< runWithOptions

processOptions :: Options -> IO ()
processOptions (Install _) = undefined
processOptions (REPL files) = repl files
processOptions (Run src) = runPlato src
processOptions Version = putStrLn $ "Plato version " ++ version