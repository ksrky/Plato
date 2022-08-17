module Plato.Debug.CoreTrans where

import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Translation.AbstractToIR
import Plato.Translation.IRToCore

import System.Console.Haskeline
import System.Environment

import Control.Monad.State

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

process :: String -> IO Commands
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                inner <- abstract2ir ast
                cmds <- ir2core emptyContext inner
                print cmds >> return cmds
