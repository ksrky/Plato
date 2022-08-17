module Main where

import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Common.Info
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Core.Syntax
import Plato.Core.Utils
import Plato.Translation.AbstractToInternal
import Plato.Translation.InternalToCore

import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                src : _ -> do
                        contents <- readFile src
                        process initContext contents
                        return ()

repl :: IO ()
repl = runInputT defaultSettings (loop initContext)
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
processFile src = undefined

process :: Context -> String -> IO Context
process ctx input = case runAlex input parse of
        Left msg -> putStrLn msg >> return ctx
        Right ast -> do
                inner <- abstract2internal ast
                cmds <- internal2core initContext inner
                let ctx' = foldr cons ctx (binds cmds)
                    res = eval ctx' (body cmds)
                putStrLn $ pretty ctx' res
                return ctx'