module Plato.Debug.REPL where

import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Common.Info
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Core.Syntax
import Plato.Core.Utils
import Plato.Debug.EvalIO
import Plato.Translation.AbstractToIR
import Plato.Translation.IRToCore

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
repl = runInputT defaultSettings (loop emptyContext)
    where
        loop ctx = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process ctx input
                                loop ctx

processFile :: String -> IO ()
processFile [] = return ()
processFile fname = do
        let src = "test/testcases/" ++ fname
        contents <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process emptyContext contents
        putStrLn ""

process :: Context -> String -> IO Context
process ctx input = case runAlex input parse of
        Left msg -> putStrLn msg >> return ctx
        Right ast -> do
                inner <- abstract2ir ast
                cmds <- ir2core ctx inner
                let ctx' = foldl (flip cons) ctx (binds cmds)
                tyT <- typeof ctx' (body cmds)
                res <- liftIO $ evalIO ctx' (body cmds)
                putStrLn $ pretty (ctx', res)
                return ctx'
