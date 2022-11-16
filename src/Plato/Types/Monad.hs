module Plato.Types.Monad where

import Plato.Core.Context
import Plato.Syntax.Typing
import Plato.Types.Fixity
import Plato.Types.Name.Global

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import Plato.Types.Name (Name)

data PlatoInfo = PInfo
        { plt_fileName :: FilePath
        , plt_isEntry :: Bool
        }

newtype PlatoStore = PStore {plt_context :: Context Name}

data PlatoState = PState
        { plt_glbNameEnv :: GlbNameEnv
        , plt_fixityEnv :: FixityEnv GlbName
        , plt_tyEnv :: TyEnv
        , plt_knEnv :: KnEnv
        , plt_glbContext :: Context GlbName
        }

initPInfo :: PlatoInfo
initPInfo =
        PInfo
                { plt_fileName = "<no file name>"
                , plt_isEntry = True
                }

initPStore :: PlatoStore
initPStore = PStore{plt_context = emptyContext}

instance Semigroup PlatoStore where
        s1 <> s2 = PStore{plt_context = plt_context s1 <> plt_context s2}

instance Monoid PlatoStore where
        mempty = PStore{plt_context = emptyContext}

initPState :: PlatoState
initPState =
        PState
                { plt_glbNameEnv = M.empty
                , plt_fixityEnv = M.empty
                , plt_tyEnv = M.empty
                , plt_knEnv = M.empty
                , plt_glbContext = emptyContext
                }

type Plato m = RWST PlatoInfo PlatoStore PlatoState m

runPlato :: Plato m a -> PlatoInfo -> PlatoState -> m (a, PlatoState, PlatoStore)
runPlato = runRWST

evalPlato :: Monad m => Plato m a -> PlatoInfo -> PlatoState -> m (a, PlatoStore)
evalPlato = evalRWST

returnPlato :: Monad m => Plato m a -> PlatoInfo -> PlatoState -> m a
returnPlato x i s = fst <$> evalRWST x i s