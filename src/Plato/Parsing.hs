module Plato.Parsing (parseProgram) where

import Plato.Common.Monad
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text qualified as T

parseProgram :: (MonadThrow m, MonadIO m) => T.Text -> Plato m Program
parseProgram inp = do
        file <- asks plt_fileName
        (program, _) <- liftIO $ parse file inp parser
        return program
