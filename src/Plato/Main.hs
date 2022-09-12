module Plato.Main where

import qualified Data.Text as T

runPlato :: String -> IO ()
runPlato src = putStrLn "Plato"

repl :: IO ()
repl = putStrLn "Plato REPL"
