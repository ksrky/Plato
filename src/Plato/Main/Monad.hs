module Plato.Main.Monad where

import Plato.Core.Context

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Parsing.Fixity (OpDict)
import Plato.Syntax.Typing
import Plato.Typing.Rename

data PlatoState = PlatoState
        { isEntry :: Bool
        , basePath :: FilePath
        , context :: Context
        , typingEnv :: TypEnv
        , renames :: Names
        , opDict :: OpDict
        , importedList :: [ModuleName]
        , importingList :: [ModuleName]
        }

initPlatoState :: PlatoState
initPlatoState =
        PlatoState
                { isEntry = True
                , basePath = "."
                , context = emptyContext
                , typingEnv = M.empty
                , renames = []
                , opDict = M.empty
                , importedList = []
                , importingList = []
                }

type Plato m a = StateT PlatoState m a
