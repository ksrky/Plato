module Plato.Interpreter where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Text qualified as T

import Plato.Common.Location
import Plato.Driver.Monad
import Plato.Nicifier.OpParser
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.PsToTyp as T
import Plato.Syntax.Core
import Plato.TypToCore as C
import Plato.Typing.Tc

interpret :: PlatoMonad m => T.Text -> m ()
interpret inp = undefined

class Interpreter e where
        enter :: (MonadIO m, MonadThrow m) => e -> Prog -> m ()
        interp :: (MonadIO m, MonadThrow m) => e -> Term -> m ()
