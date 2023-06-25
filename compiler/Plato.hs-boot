module Plato where

import Plato.Driver.Monad

compileToCore :: PlatoMonad m => FilePath -> m ()