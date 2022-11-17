module Plato.Types.Monad where

import Plato.Core.Context
import Plato.Syntax.Typing
import Plato.Types.Fixity
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data PlatoInfo = PInfo
        { plt_fileName :: FilePath
        , plt_isEntry :: Bool
        , plt_importingList :: S.Set ModuleName
        }

data PlatoStore = PStore {}

data PlatoState = PState
        { plt_glbNameEnv :: GlbNameEnv
        , plt_fixityEnv :: FixityEnv GlbName
        , plt_tyEnv :: TyEnv
        , plt_knEnv :: KnEnv
        , plt_glbContext :: Context
        , plt_importedList :: S.Set ModuleName
        }

initPInfo :: PlatoInfo
initPInfo =
        PInfo
                { plt_fileName = "<no file name>"
                , plt_isEntry = True
                , plt_importingList = S.empty
                }

initPStore :: PlatoStore
initPStore = PStore{}

instance Semigroup PlatoStore where
        _ <> _ = PStore{}

instance Monoid PlatoStore where
        mempty = PStore{}

initPState :: PlatoState
initPState =
        PState
                { plt_glbNameEnv = M.empty
                , plt_fixityEnv = M.empty
                , plt_tyEnv = M.empty
                , plt_knEnv = M.empty
                , plt_glbContext = emptyContext
                , plt_importedList = S.empty
                }

type Plato m = RWST PlatoInfo PlatoStore PlatoState m

evalPlato :: Monad m => Plato m a -> PlatoInfo -> PlatoState -> m (a, PlatoStore)
evalPlato = evalRWST

returnPlato :: Monad m => Plato m a -> PlatoInfo -> PlatoState -> m a
returnPlato x i s = fst <$> evalRWST x i s