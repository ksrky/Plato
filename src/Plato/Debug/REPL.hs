module Plato.Debug.REPL where

import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Debug.EvalIO
import Plato.Translation.AbsToTyp
import Plato.Translation.SrcToAbs
import Plato.Translation.TypToCore

import Control.Exception.Safe
import Control.Monad.State
import Plato.Typing.Rename
import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                fname : _ -> void $ processFile emptyContext fname

debugPlato :: String -> IO ()
debugPlato src = do
        ctx <- importBase emptyContext
        void $ processFile emptyContext src

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
        typ <- abs2typ emptyMemo ast
        cmds <- typ2core ctx typ
        -- processing imports
        {-ctx' <- (`execStateT` ctx) $
                forM (imports cmds) $ \modn -> do
                        ctx <- get
                        ctx' <- lift $ processFile ctx (toPath modn)
                        put ctx'
        let cmds' = commandsShift (length ctx' - length ctx) cmds-}
        ctx' <- (`execStateT` ctx) $
                forM_ (binds cmds) $ \(fi, (x, bind)) -> do
                        ctx <- get
                        -- checkBinding fi ctx bind
                        put $ cons (x, bind) ctx
        tyT <- typeof ctx' (fst $ body cmds)
        res <- evalIO ctx' (fst $ body cmds)
        putStrLn $ pretty (ctx', res)
        return ctx'

importBase :: (MonadThrow m, MonadIO m) => Context -> m Context
importBase ctx = (`execStateT` ctx) $
        forM baseModules $ \modn -> StateT $ \ctx -> do
                let src = toBasePath modn
                ctx' <- liftIO $ processFile ctx src
                return ((), ctx')