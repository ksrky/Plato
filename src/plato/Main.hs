module Main where

import Config
import Options
import Plato

main :: IO ()
main = processOptions =<< runWithOptions

processOptions :: Options -> IO ()
processOptions (Install _) = undefined
processOptions (REPL _files) = undefined
processOptions (Run src) = runPlato src
processOptions Version = putStrLn $ "Plato version " ++ cfg_version config