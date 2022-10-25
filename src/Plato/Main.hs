module Plato.Main where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Syntax.Core
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Console.Haskeline

runPlato :: String -> IO ()
runPlato = processFile emptyContext

repl :: IO ()
repl = runInputT defaultSettings (loop emptyContext)
    where
        loop ctx = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just input -> do
                                liftIO $ process ctx (T.pack input)
                                loop ctx

processFile :: Context -> String -> IO ()
processFile ctx src = do
        input <- T.readFile src
        _ <- process ctx input
        return () --tmp

processCommand :: (MonadThrow m, MonadIO m) => Context -> Command -> m Context
processCommand ctx Import{} = return ctx
processCommand ctx (Bind name bind) = return $ V.cons (unLoc name, bind) ctx
processCommand ctx (Eval t) = do
        liftIO $ print $ eval ctx (unLoc t)
        return ctx

process :: (MonadThrow m, MonadIO m) => Context -> T.Text -> m Context
process ctx input = do
        ps <- src2ps input
        typ <- ps2typ ps
        cmds <- typ2core ctx typ
        foldM processCommand ctx cmds
