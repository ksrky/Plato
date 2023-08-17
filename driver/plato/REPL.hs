module REPL (repl) where

import Control.Exception.Safe
import Control.Monad.Trans
import Data.Text qualified as T
import System.Console.Haskeline

import Plato
import Plato.Interactive

repl :: [FilePath] -> Session -> IO ()
repl files session =  
        unPlato (do
                _ <- mapM_ compileToCore files
                runInteractive (runInputT defaultSettings loop)
                ) session 

loop :: (PlatoMonad m, MonadMask m) => InputT (Interactive m) ()
loop = do
        minp <- getInputLine ">> "
        case minp of
                Nothing -> return ()
                Just "" -> loop
                Just (':' : cmd) -> replCommand cmd
                Just inp -> lift (evaluateCore (T.pack inp)) >> loop

replCommand :: String -> (PlatoMonad m, MonadMask m) => InputT (Interactive m) ()
replCommand "q" = return ()
replCommand _ = loop