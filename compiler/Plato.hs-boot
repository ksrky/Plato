module Plato where

import Plato.Driver.Monad
import Plato.Syntax.Core

compileToCore :: PlatoMonad m => FilePath -> m Prog