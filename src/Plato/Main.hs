module Plato.Main (process) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text qualified as T

import Plato.Common.Debug
import Plato.Common.Uniq
import Plato.Parsing
import Plato.PsToTyp
import Plato.Scoping
import Plato.Typing

data PlatoContext = PContext
data PlatoState = PState

type Plato m a = RWST PlatoContext () PlatoState m a

instance HasInfo PlatoContext where
        getFilePath PContext = "" -- tmp

instance HasScope PlatoState where
        getScope PState = undefined -- tmp
        modifyScope = undefined

instance HasUniq PlatoContext where
        getUniq = undefined

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        pssyn <- parseProgram input
        pssyn' <- scopingProgram pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typingProgram typsyn
        -- coresyn <- typ2core typsyn'
        -- coresyn' <- corered coresyn
        -- printResult coresyn'
        undefined