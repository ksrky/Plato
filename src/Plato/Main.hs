module Plato.Main where

import Plato.Common.Error
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline

runPlato :: String -> IO ()
runPlato src = do
        processFile src

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> return ()
                        Just "" -> return ()
                        Just input -> do
                                liftIO $ process (T.pack input)
                                loop

processFile :: String -> IO ()
processFile src = do
        input <- T.readFile src
        process input

process :: T.Text -> IO ()
process input = catchError $ do
        ps <- src2ps input
        typ <- ps2typ ps
        print ps
