module Plato.PsToTyp.Graph (
        ScopeGraph,
        initScopeGraph,
        newRoot,
        pushNewScope,
        scoping,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.IORef
import Data.Map.Strict qualified as M
import GHC.Stack

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Name
import Plato.PsToTyp.Utils

type Scope = M.Map Name Ident

newScope :: [Ident] -> Scope
newScope = M.fromList . map (\id -> (nameIdent id, id))

type Graph = M.Map Ident [Ident]

data ScopeGraph = ScopeGraph
        { roots :: ![Ident] -- the current definitions
        , scopes :: ![Scope] -- the current hierarchical scopes
        , graph :: !(IORef Graph)
        }

initScopeGraph :: MonadIO m => m ScopeGraph
initScopeGraph = do
        ref <- liftIO $ newIORef M.empty
        return $ ScopeGraph{roots = [], scopes = [], graph = ref}

newRoot :: Ident -> ScopeGraph -> ScopeGraph
newRoot id env = env{roots = id : roots env}

pushNewScope :: HasDomain a => a -> ScopeGraph -> ScopeGraph
pushNewScope seq env = env{scopes = newScope (getDomain seq) : scopes env}

addEdge :: MonadIO m => Ident -> Ident -> ScopeGraph -> m ()
addEdge u v env = do
        es <- liftIO $ readIORef (graph env)
        let es' = M.insertWith (++) u [v] es
        liftIO $ writeIORef (graph env) es'

scoping :: forall e m. (HasCallStack, MonadReader e m, MonadIO m) => Ident -> ScopeGraph -> m ()
scoping id env = do
        loop id (roots env) (scopes env)
    where
        loop :: Ident -> [Ident] -> [Scope] -> m ()
        loop _ [] [] = return ()
        loop id (r : rs) (sc : scs) = do
                case M.lookup (nameIdent id) sc of
                        Just id_def -> addEdge r id_def env
                        Nothing -> loop id rs scs
        loop _ _ _ = unreachable "stack size mismatched"
