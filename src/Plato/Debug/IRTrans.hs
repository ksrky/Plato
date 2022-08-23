module Plato.Debug.IRTrans where

import Plato.Common.Pretty
import Plato.Translation.AbsToInt
import Plato.Translation.SrcToAbs

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
        let src = "test/testcases/" ++ fname
        contents <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process contents
        putStrLn ""

process :: String -> IO ()
process input = do
        ast <- src2abs input
        ir <- abs2int ast
        putStrLn $ pretty ir
