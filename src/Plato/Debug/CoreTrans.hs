module Plato.Debug.CoreTrans where

import Plato.Common.Pretty
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Utils
import Plato.Translation.AbsToInt
import Plato.Translation.IntToCore
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

process :: String -> IO Commands
process input = do
        ast <- src2abs input
        ir <- abs2int ast
        cmds <- int2core emptyContext ir
        putStrLn (pretty (emptyContext, cmds)) >> return cmds
