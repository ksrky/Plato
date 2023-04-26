module Plato.Parsing (parser) where

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Monad
import Plato.Common.Name

import Plato.Parsing.Monad
import Plato.Parsing.Parser

import Plato.Common.Location
import Plato.Syntax.Parsing

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Text as T

parser :: (MonadThrow m, MonadIO m) => T.Text -> Plato m Program
parser inp = do
        file <- asks plt_fileName
        (program, st) <- liftIO $ parse file inp parser
        return program
