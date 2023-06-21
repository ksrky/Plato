module Main where

import Options
import Plato

main :: IO ()
main = processOptions =<< runWithOptions

processOptions :: Options -> IO ()
processOptions (REPL _files) = error "not implemented"
processOptions (Run src libs mblog) = do
        session <- initSession
        setInfo src libs mblog session
        runPlato src session
processOptions (Version version) = putStrLn $ "Plato version " ++ version