module Plato.Interaction.Status where

import Plato.Core.Context
import Plato.Interaction.Module

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Syntax.Typing

data PlatoState = PlatoState
        { basePath :: String
        , context :: Context
        , typingEnv :: TypEnv
        , importedList :: [ModuleName]
        , importingList :: [ModuleName]
        }

initPlatoState :: String -> PlatoState
initPlatoState src =
        PlatoState
                { basePath = getBasePath src
                , context = emptyContext
                , typingEnv = M.empty
                , importedList = []
                , importingList = []
                }
    where
        getBasePath :: String -> String
        getBasePath [] = ""
        getBasePath ('/' : xs) = xs
        getBasePath (x : xs) = getBasePath xs

type Plato m a = StateT PlatoState m a
