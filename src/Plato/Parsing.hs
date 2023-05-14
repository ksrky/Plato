module Plato.Parsing (parseProgram) where

import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text qualified as T

import Plato.Common.Debug
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

parseProgram :: (MonadReader env m, HasInfo env, MonadIO m) => T.Text -> m Program
parseProgram inp = do
        file <- asks getFilePath
        (program, _) <- liftIO $ parse file inp parser
        return program