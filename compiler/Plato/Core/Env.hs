module Plato.Core.Env
    ( CoreEnv
    , EnvEntries
    , EnvEntry (..)
    , HasCoreEnv (..)
    , PrtInfo (..)
    , extE
    , getE
    , initCoreEnv
    , prtE
    , restoreScope
    , setE
    ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict        qualified as M
import Data.Vector            qualified as V

import Plato.Common.Ident
import Plato.Core.Closure
import Plato.Syntax.Core

data EnvEntry
    = Index Ix
    | Closure (Clos Term)
    deriving (Show)

data PrtInfo = PrtInfo
    { prt_ident  :: Ident
    , prt_expand :: Bool
    }

type EnvEntries = V.Vector (EnvEntry, PrtInfo)

type CoreEnv = (IORef EnvEntries)

initCoreEnv :: IO (IORef EnvEntries)
initCoreEnv = newIORef mempty

class HasCoreEnv e where
    getCoreEnv :: (MonadIO m) => e -> m (IORef EnvEntries)
    setCoreEnv :: (MonadIO m) => e -> EnvEntries -> m ()
    setCoreEnv e envent = do
        ref <-  getCoreEnv e
        liftIO $ writeIORef ref envent
    readCoreEnv :: (MonadIO m) => e -> m EnvEntries
    readCoreEnv ref = liftIO . readIORef =<< getCoreEnv ref

instance HasCoreEnv CoreEnv where
    getCoreEnv = return

extE :: (HasCoreEnv e, MonadIO m) => PrtInfo -> e -> m Ix
extE fi ref = do
    env <- readCoreEnv ref
    let i = length env
    setCoreEnv ref (env `V.snoc` (Index i, fi))
    return i

getE :: (HasCoreEnv e, MonadIO m) => Ix -> e -> m EnvEntry
getE i ref = do
    env <- readCoreEnv ref
    return $ fst $ env V.! i

setE :: (HasCoreEnv e, MonadIO m) => Ix -> EnvEntry -> e -> m ()
setE i v ref = do
    env <- readCoreEnv ref
    setCoreEnv ref (env V.// [(i, (v, snd $ env V.! i))])

prtE :: (HasCoreEnv e, MonadIO m) => Ix -> e -> m PrtInfo
prtE i ref = do
    env <- readCoreEnv ref
    return $ snd $ env V.! i

restoreScope :: EnvEntries -> Scope
restoreScope =
    Scope . M.fromList . zipWith (\i (_, fi) -> (prt_ident fi, (i, Nothing))) [0 ..] . V.toList
