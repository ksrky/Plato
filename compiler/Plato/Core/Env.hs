module Plato.Core.Env where

import Control.Monad.IO.Class
import Data.IORef

import Plato.Core.Closure
import Plato.Core.Data

type EnvEntries = [(EnvEntry, PrtInfo)]

class Env e where
        extE :: MonadIO m => PrtInfo -> e -> m Index
        getE :: MonadIO m => Index -> e -> m EnvEntry
        setE :: MonadIO m => Index -> EnvEntry -> e -> m ()
        prtE :: MonadIO m => Index -> e -> m PrtInfo

instance Env (IORef EnvEntries) where
        extE fi ref = do
                env <- liftIO $ readIORef ref
                let i = length env
                liftIO $ writeIORef ref (env ++ [(Index i, fi)])
                return i
        getE i ref = do
                env <- liftIO $ readIORef ref
                return $ fst $ env !! i
        setE i v ref = do
                env <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (set env i (v, snd (env !! i)))
            where
                set :: [a] -> Int -> a -> [a]
                set [] _ _ = error "list is empty"
                set (_ : as) 0 b = b : as
                set (a : as) i b = a : set as (i - 1) b
        prtE i ref = do
                env <- liftIO $ readIORef ref
                return $ snd $ env !! i