module Plato.Main where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Translation.AbsToTyp
import Plato.Translation.SrcToAbs
import Plato.Translation.TypToCore

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import System.Console.Haskeline

data Status = Status {ismain :: Bool, debug :: Bool}

defaultStatus :: Status
defaultStatus = Status{ismain = True, debug = False}

runPlato :: Status -> String -> IO ()
runPlato sts src = do
        ctx <- importBase emptyContext
        void $ processFile sts emptyContext src

repl :: IO ()
repl = runInputT defaultSettings (loop emptyContext)
    where
        loop ctx = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process defaultStatus ctx input
                                loop ctx

processFile :: Status -> Context -> String -> IO Context
processFile sts ctx src = do
        input <- readFile src
        process sts ctx input

process :: Status -> Context -> String -> IO Context
process sts ctx input = do
        ast <- src2abs input
        typ <- abs2typ ast
        cmds <- typ2core ctx typ
        -- processing imports
        {-ctx' <- (`execStateT` ctx) $
                forM (imports cmds) $ \modn -> do
                        ctx <- get
                        ctx' <- lift $ processFile sts{ismain = False} ctx (toPath modn)
                        put ctx'-}
        let cmds' = commandsShift (length ctx - length ctx) cmds
        ctx' <- (`execStateT` ctx) $
                forM_ (binds cmds') $ \(fi, (x, bind)) -> do
                        ctx <- get
                        checkBinding fi ctx bind
                        put $ cons (x, bind) ctx
        when (null ctx) $ do
                -- tmp: when ismain
                tyT <- typeof ctx' (body cmds)
                let res = eval ctx' (body cmds)
                putStrLn $ pretty (ctx', res)
        return ctx'

importBase :: (MonadThrow m, MonadIO m) => Context -> m Context
importBase ctx = (`execStateT` ctx) $
        forM baseModules $ \modn -> StateT $ \ctx -> do
                let src = toBasePath modn
                ctx' <- liftIO $ processFile defaultStatus{ismain = False} ctx src
                return ((), ctx')