module Main where

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
import Plato.Common.Info
import Plato.Syntax.Canon
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

processCommand :: Command -> State Context ()
processCommand (Import mod) = undefined
processCommand (Bind x bind) = state $ \ctx ->
        case name2index dummyInfo ctx x of
                Just idx -> ((), cons (x, bind) (update idx (x, bind) ctx))
                Nothing -> ((), cons (x, bind) ctx)
processCommand (Eval t) = return ()

processEval :: Context -> [Command] -> Maybe Term
processEval ctx (Eval t : _) = Just $ eval ctx t
processEval ctx (cmd : cmds) = processEval ctx cmds
processEval ctx _ = Nothing

process :: Context -> String -> IO Context
process ctx input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                ast' <- reorganize ast
                cmds <- execWriterT (transProgram ast') `evalStateT` ctx
                let ctx' = mapM processCommand cmds `execState` ctx
                    res = processEval ctx' cmds
                    ppres = pretty <$> res
                print ppres
                return ctx'
