module Plato.Core.Env where

import Control.Monad.IO.Class
import Data.IORef
import Data.Vector qualified as V

import Plato.Common.Ident
import Plato.Core.Closure
import Plato.Syntax.Core

data EnvEntry
        = Index Index
        | Closure (Clos Term)
        deriving (Show)

data PrtInfo = PrtInfo
        { name :: Ident
        , expand :: Bool
        }

type EnvEntries = V.Vector (EnvEntry, PrtInfo)

class CoreEnv e where
        extE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: MonadIO m => Index -> e -> m EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()
        prtE :: MonadIO m => Index -> e -> m PrtInfo

instance CoreEnv (IORef EnvEntries) where
        extE fi ref = do
                env <- liftIO $ readIORef ref
                let i = length env
                liftIO $ writeIORef ref (env `V.snoc` (Index i, fi))
                return i
        getE i ref = do
                env <- liftIO $ readIORef ref
                return $ fst $ env V.! i
        setE i v ref = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (env V.// [(i, (v, snd $ env V.! i))])
        prtE i ref = do
                env <- liftIO $ readIORef ref
                return $ snd $ env V.! i