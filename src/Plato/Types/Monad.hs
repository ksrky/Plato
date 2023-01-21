module Plato.Types.Monad where

import Plato.Core.Context
import Plato.Syntax.Typing
import Plato.Types.Fixity
import Plato.Types.Name
import Plato.Types.Name.Global

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.FilePath

----------------------------------------------------------------
-- Plato Monad
----------------------------------------------------------------
data PlatoInfo = PInfo
        { plt_basePath :: FilePath
        , plt_fileName :: FilePath
        , plt_isEntry :: Bool
        , plt_importingList :: S.Set ModuleName
        }
        deriving (Show)

data PlatoStore = PStore {} deriving (Show)

data PlatoState = PState
        { plt_glbEnv :: GlbEnv
        , plt_fixityEnv :: FixityEnv GlbName
        , plt_tyEnv :: TyEnv
        , plt_knEnv :: KnEnv
        , plt_glbContext :: Context
        , plt_importedList :: S.Set ModuleName
        }
        deriving (Show)

initPInfo :: PlatoInfo
initPInfo =
        PInfo
                { plt_basePath = ""
                , plt_fileName = "<no file name>"
                , plt_isEntry = True
                , plt_importingList = S.empty
                }

initPInfo' :: FilePath -> PlatoInfo
initPInfo' src =
        initPInfo
                { plt_basePath = takeDirectory src
                , plt_fileName = src
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
                { plt_glbEnv = M.empty
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

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------
getModPath :: Monad m => ModuleName -> Plato m FilePath
getModPath modn = do
        base_path <- asks plt_basePath
        return (base_path </> mod2path modn)
