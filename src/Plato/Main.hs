module Plato.Main (process) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text qualified as T

import Plato.Common.Debug
import Plato.Parsing
import Plato.Scoping

data PlatoContext = PContext
data PlatoState = PState

type Plato m a = RWST PlatoContext () PlatoState m a

instance HasInfo PlatoContext where
        getFilePath PContext = "" -- tmp

instance HasScope PlatoState where
        getScope PState = undefined -- tmp
        modifyScope = undefined

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        pssyn <- parseProgram input
        pssyn' <- scopingProgram pssyn
        undefined