module Plato.Main.Monad where

import Plato.Core.Context

import Control.Monad.RWS
import Control.Monad.State
import qualified Data.Map.Strict as M
import Plato.Common.Name
import Plato.Parsing.Fixity (OpTable)
import Plato.Syntax.Typing
import Plato.Typing.Rename

data PlatoState = PlatoState
        { isEntry :: Bool
        , basePath :: FilePath
        , context :: Context
        , typingEnv :: TypTable
        , renames :: NameTable
        , opTable :: OpTable
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
                , renames = M.empty
                , opTable = M.empty
                , importedList = []
                , importingList = []
                }

type Plato m a = StateT PlatoState m a

data PR = PR
        { pr_isEntry' :: Bool
        , pr_moduleName :: ModuleName
        }

data PW = PW
        { pw_opTable :: OpTable
        , pw_typTable :: TypTable
        , pw_nameTable :: NameTable
        }

data PS = PS
        { ps_importedList :: [ModuleName]
        , ps_importingList :: [ModuleName]
        }

type Plato' m a = RWST () () PlatoState m a