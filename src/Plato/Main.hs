module Plato.Main (processFile) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.RWS
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Plato.Common.Debug
import Plato.Common.Uniq
import Plato.Core.Env
import Plato.Parsing
import Plato.PsToTyp
import Plato.RunCore
import Plato.Scoping
import Plato.TypToCore
import Plato.Typing
import Plato.Typing.Monad

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

instance HasEnv PlatoState where
        getEnv = undefined
        modifyEnv = undefined

instance HasCoreEnv PlatoState where
        getEnv = undefined
        modifyEnv = undefined

process :: (MonadThrow m, MonadIO m) => T.Text -> Plato m ()
process input = do
        pssyn <- parseProgram input
        pssyn' <- scopingProgram pssyn
        typsyn <- ps2typ pssyn'
        typsyn' <- typingProgram typsyn
        coresyn <- typ2core typsyn'
        coresyn' <- runCore coresyn
        liftIO $ mapM_ printResult coresyn'

processFile :: (MonadThrow m, MonadIO m) => FilePath -> Plato m ()
processFile src = do
        input <- liftIO $ T.readFile src
        process input