module Plato.Debug.ParseREPL where

import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Translation.AbstractToInternal

import Control.Monad.State
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                fname : _ -> processFile fname

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process input
                                loop

processFile :: String -> IO ()
processFile [] = return ()
processFile fname = do
        let src = "examples/" ++ fname
        contents <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process contents
        putStrLn ""

process :: String -> IO ()
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                inner <- abstract2internal ast
                print inner
