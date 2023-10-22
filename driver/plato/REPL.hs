module REPL (repl) where

import Control.Exception.Safe
import Control.Monad.Trans.Class
import Data.Text qualified as T
import System.Console.Haskeline

import Plato

repl :: [FilePath] -> Session -> IO ()
repl files = unPlato $ do
        mapM_ compileToCore files
        runInteractive (runInputT defaultSettings loop)

loop :: (PlatoMonad m, MonadMask m) => InputT (Interactive m) ()
loop = do
        minp <- getInputLine ">> "
        case minp of
                Nothing -> return ()
                Just "" -> loop
                Just (':' : cmd) -> replCommand cmd
                Just inp -> do
                        lift $ evaluateCore (T.pack inp)
                        loop

replCommand :: String -> (PlatoMonad m, MonadMask m) => InputT (Interactive m) ()
replCommand "q" = return ()
replCommand _ = loop