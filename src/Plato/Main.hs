module Plato.Main where

import Plato.Common.Error
import Plato.Common.Info
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
import Plato.Typing.Rename

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import System.Console.Haskeline

data Status = Status {ismain :: Bool, debug :: Bool, source :: String}

defaultStatus :: Status
defaultStatus = Status{ismain = True, debug = False, source = ""}

runPlato :: Status -> String -> IO ()
runPlato sts src = do
        (ctx, memo) <- importBase emptyContext emptyMemo
        void $ processFile sts ctx memo src

debugPlato :: Status -> String -> IO ()
debugPlato sts src = do
        (ctx, memo) <- importBase emptyContext emptyMemo
        void $ processFile sts ctx memo src

repl :: IO ()
repl = runInputT defaultSettings (loop emptyContext)
    where
        loop ctx = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process defaultStatus ctx emptyMemo input
                                loop ctx

processFile :: Status -> Context -> Memo -> String -> IO (Context, Memo)
processFile sts ctx memo src = do
        input <- readFile src
        process sts ctx memo input

process :: Status -> Context -> Memo -> String -> IO (Context, Memo)
process sts ctx memo input = do
        ast <- src2abs input
        typ <- abs2typ memo ast
        cmds <- typ2core ctx typ
        -- processing imports
        {-ctx' <- (`execStateT` ctx) $
                forM (imports cmds) $ \modn -> do
                        ctx <- get
                        ctx' <- lift $ processFile sts{ismain = False} ctx (toPath modn)
                        put ctx'
        let cmds' = commandsShift (length ctx - length ctx) cmds-}
        ctx' <- (`execStateT` ctx) $
                forM_ (binds cmds) $ \(fi, (x, bind)) -> do
                        ctx <- get
                        -- checkBinding fi ctx bind
                        put $ cons (x, bind) ctx
        when (ismain sts) $ do
                tyT <- typeof ctx' (fst $ body cmds)
                res <-
                        if debug sts
                                then evalIO ctx' (fst $ body cmds)
                                else return $ eval ctx' (fst $ body cmds)
                putStrLn $ pretty (ctx', res)
        let (funcns, bind) = getBodyBind cmds
            rcd = str2varName $ source sts
            memo' = memo{store = foldr cons (store memo) (zip funcns (repeat rcd))}
        ctx'' <- addBinding dummyInfo rcd bind ctx'
        return (ctx'', memo')

importBase :: (MonadThrow m, MonadIO m) => Context -> Memo -> m (Context, Memo)
importBase ctx memo = (`execStateT` (ctx, memo)) $
        forM baseModules $ \modn -> StateT $ \(ctx, memo) -> do
                let src = toBasePath modn
                -- liftIO $ putStrLn src
                -- liftIO $ print ctx
                (ctx', memo') <- liftIO $ processFile defaultStatus{ismain = False, source = src} ctx memo src
                return ((), (ctx', memo'))
