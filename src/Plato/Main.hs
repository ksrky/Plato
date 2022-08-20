module Plato.Main where

import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Translation.AbsToIR
import Plato.Translation.IRToCore
import Plato.Translation.SrcToAbs

import Control.Monad.State
import System.Console.Haskeline

runPlato :: String -> IO ()
runPlato = void . processFile emptyContext

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
processFile ctx src = do
        input <- readFile src
        process ctx input

process :: Context -> String -> IO Context
process ctx input = do
        ast <- src2abs input
        inner <- abs2ir ast
        cmds <- ir2core ctx inner
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
                let res = eval ctx'' (body cmds)
                putStrLn $ pretty (ctx'', res)
        return ctx''
