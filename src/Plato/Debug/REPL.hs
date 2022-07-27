module Plato.Debug.REPL where

import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Evaluate
import Plato.Core.Pretty
import Plato.Core.Syntax
import Plato.Syntax.Lexer
import Plato.Syntax.Parser
import Plato.Translation.AbstractToCore

import Control.Monad.State
import Control.Monad.Writer.Lazy
import Plato.Debug.EvaluateIO
import Plato.Syntax.Canon
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
        putStrLn $ "----------" ++ fname ++ "----------"
        process initContext contents
        putStrLn ""

processCommand :: Command -> State Context ()
processCommand (Import mod) = undefined
processCommand (Bind x bind) = state $ \ctx ->
        case getVarIndex x ctx of
                Just idx -> ((), cons (x, bind) (update idx (x, bind) ctx))
                Nothing -> ((), cons (x, bind) ctx)
processCommand (Eval t) = return ()

processEval :: Context -> [Command] -> IO (Maybe Term)
processEval ctx (Eval t : _) = Just <$> evalIO ctx t
processEval ctx (cmd : cmds) = processEval ctx cmds
processEval ctx _ = return Nothing

process :: Context -> String -> IO Context
process ctx input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                ast' <- reorganize ast
                cmds <- execWriterT (transProgram ast') `evalStateT` ctx
                let ctx' = mapM processCommand cmds `execState` ctx
                res <- processEval ctx' cmds
                print (pretty <$> res)
                return ctx'