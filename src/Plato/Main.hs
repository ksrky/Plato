module Plato.Main where

import Plato.Common.Error
import Plato.Core.Context
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
        process ctx input

process :: Context -> T.Text -> IO ()
process ctx input = catchError $ do
        ps <- src2ps input
        typ <- ps2typ ps
        cmds <- typ2core ctx typ
        print ps
