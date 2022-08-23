module Plato.Debug.REPL where

import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Debug.EvalIO
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
                fname : _ -> void $ processFile emptyContext fname

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

processFile :: Context -> String -> IO Context
processFile ctx fname = do
        let src = "test/testcases/" ++ fname
        input <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process ctx input

process :: Context -> String -> IO Context
process ctx input = do
        ast <- src2abs input
        ir <- abs2int ast
        cmds <- int2core ctx ir
        -- processing imports
        ctx' <- (`execStateT` ctx) $
                forM (imports cmds) $ \modn -> do
                        ctx <- get
                        ctx' <- lift $ processFile ctx (toPath modn)
                        put ctx'
        let cmds' = commandsShift (length ctx' - length ctx) cmds
        ctx'' <- (`execStateT` ctx') $
                forM_ (binds cmds) $ \(fi, (x, bind)) -> do
                        ctx <- get
                        checkBinding fi ctx bind
                        put $ cons (x, bind) ctx
        when (null ctx) $ do
                tyT <- typeof ctx'' (body cmds)
                res <- evalIO ctx'' (body cmds)
                putStrLn $ pretty (ctx'', res)
        return ctx''
