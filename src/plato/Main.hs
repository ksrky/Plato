module Main where

import Config
import Options
import Plato

main :: IO ()
main = processOptions =<< runWithOptions

processOptions :: Options -> IO ()
processOptions (REPL _files) = error "not implemented"
processOptions (Run src) = runPlato src
processOptions Version = putStrLn $ "Plato version " ++ cfg_version config