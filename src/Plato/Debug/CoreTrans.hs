module Plato.Debug.CoreTrans where

import Plato.Common.Pretty
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Utils
import Plato.Translation.AbsToIR
import Plato.Translation.IRToCore
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
        inner <- abs2ir ast
        cmds <- ir2core emptyContext inner
        putStrLn (pretty (emptyContext, cmds)) >> return cmds
