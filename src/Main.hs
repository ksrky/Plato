module Main where

import CLI.Constant (version)
import CLI.Options (Options (..), runWithOptions)

-- import Plato.Main (repl, runPlato)

main :: IO ()
main = undefined -- processOptions =<< runWithOptions
{-}
processOptions :: Options -> IO ()
processOptions (Install _) = undefined
processOptions (REPL files) = repl files
processOptions (Run src) = runPlato src
processOptions Version = putStrLn $ "Plato version " ++ version-}