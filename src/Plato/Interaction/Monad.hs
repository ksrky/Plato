module Plato.Interaction.Monad where

import Plato.Core.Context

import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Syntax.Typing
import Plato.Typing.Renamer

data PlatoState = PlatoState
        { basePath :: String
        , context :: Context
        , typingEnv :: TypEnv
        , renames :: Names
        , importedList :: [ModuleName]
        , importingList :: [ModuleName]
        }

initPlatoState :: String -> PlatoState
initPlatoState src =
        PlatoState
                { basePath = reverse $ getBasePath $ reverse src
                , context = emptyContext
                , typingEnv = M.empty
                , renames = []
                , importedList = []
                , importingList = []
                }
    where
        getBasePath :: String -> String
        getBasePath [] = ""
        getBasePath ('/' : xs) = '/' : xs
        getBasePath (x : xs) = getBasePath xs

type Plato m a = StateT PlatoState m a
