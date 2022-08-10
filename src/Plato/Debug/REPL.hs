module Plato.Debug.REPL where

{-}
import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Common.Info
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Evaluate
import Plato.Core.Syntax
import Plato.Core.Utils
import Plato.Debug.Evaluate
import Plato.Translation.AbstractToInternal
import Plato.Translation.InternalToCore

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
processFile [] = return ()
processFile fname = do
        let src = "examples/" ++ fname
        contents <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process initContext contents
        putStrLn ""

processCommand :: Command -> State Context ()
processCommand (Import mod) = undefined
processCommand (Bind x bind) = state $ \ctx ->
        case name2index dummyInfo ctx x of
                Just i -> ((), cons (x, bind) (update i (x, bindingShift (- i - 1) bind) ctx))
                Nothing -> ((), cons (x, bind) ctx)
processCommand (Eval t) = return ()

processEval :: Context -> [Command] -> IO (Maybe Term)
processEval ctx (Eval t : _) = Just <$> evalIO ctx t
processEval ctx (cmd : cmds) = processEval ctx cmds
processEval ctx _ = return Nothing

process :: Context -> String -> IO Context
process ctx input = case runAlex input parse of
        Left msg -> putStrLn msg >> return ctx
        Right ast -> do
                inner <- abstract2internal ast
                cmds <- internal2core initContext inner
                let ctx' = mapM processCommand cmds `execState` ctx
                res <- processEval ctx' cmds
                print (pretty <$> res)
                return ctx'
                -}