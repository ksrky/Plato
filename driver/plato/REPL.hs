module REPL (repl) where

import Control.Exception.Safe
import Control.Monad.Trans
import Data.Text qualified as T
import System.Console.Haskeline

import Plato

repl :: [FilePath] -> Session -> IO ()
repl files session = do
        unPlato (mapM_ compileToCore files) session
        unPlato (runInputT defaultSettings loop) session

loop :: (PlatoMonad m, MonadMask m) => InputT m ()
loop = do
        minp <- getInputLine ">> "
        case minp of
                Nothing -> return ()
                Just "" -> loop
                Just (':' : cmd) -> replCommand cmd
                Just inp -> lift (interpretExpr (T.pack inp)) >> loop

replCommand :: String -> (PlatoMonad m, MonadMask m) => InputT m ()
replCommand "q" = return ()
replCommand _ = loop