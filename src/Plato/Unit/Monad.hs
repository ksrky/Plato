module Plato.Unit.Monad where

import Plato.Core.Context

import Plato.Common.Name
import Plato.Parsing.Fixity (OpTable)
import Plato.Syntax.Typing
import Plato.Typing.Rename

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data PlatoInfo = PInfo
        { pr_isEntry :: Bool
        , pr_importingSet :: S.Set ModuleName
        }

newtype PlatoStore = PStore
        { pw_nameTable :: NameTable
        }

data PlatoState = PState
        { ps_opTable :: OpTable
        , ps_typTable :: TypTable
        , ps_context :: Context
        , ps_importedSet :: S.Set ModuleName
        }

initPInfo :: PlatoInfo
initPInfo =
        PInfo
                { pr_isEntry = True
                , pr_importingSet = S.empty
                }

initPStore :: PlatoStore
initPStore = PStore{pw_nameTable = M.empty}

instance Semigroup PlatoStore where
        ps1 <> ps2 = PStore{pw_nameTable = pw_nameTable ps1 <> pw_nameTable ps2}

instance Monoid PlatoStore where
        mempty = PStore{pw_nameTable = M.empty}

initPState :: PlatoState
initPState =
        PState
                { ps_opTable = M.empty
                , ps_typTable = M.empty
                , ps_context = emptyContext
                , ps_importedSet = S.empty
                }

type PlatoM m = RWST PlatoInfo PlatoStore PlatoState m

runPlatoM :: PlatoM m a -> PlatoInfo -> PlatoState -> m (a, PlatoState, PlatoStore)
runPlatoM = runRWST

evalPlatoM :: Monad m => PlatoM m a -> PlatoInfo -> PlatoState -> m (a, PlatoStore)
evalPlatoM = evalRWST