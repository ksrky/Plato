{-# LANGUAGE FlexibleInstances #-}

module Plato.Common.Global where

import Control.Monad.IO.Class
import Data.IORef

data Global = Global {glbUnique :: !(IORef Unique), glbUniqueStack :: ![Unique], glbLog :: !String}

initGlobal :: IO Global
initGlobal = do
        ref <- initUnique
        return Global{glbUnique = ref, glbUniqueStack = [], glbLog = ""}

-- | Unique
type Unique = Int

class HasUnique a where
        getUnique :: MonadIO m => a -> m (IORef Unique)
        pickUnique :: MonadIO m => a -> m Unique
        pickUnique env = do
                ref <- getUnique env
                u <- liftIO $ readIORef ref
                liftIO $ writeIORef ref (u + 1)
                return u

instance HasUnique (IORef Unique) where
        getUnique = return

instance HasUnique Global where
        getUnique = return . glbUnique

initUnique :: MonadIO m => m (IORef Unique)
initUnique = liftIO $ newIORef 0

newUniqueScope :: Global -> IO Global
newUniqueScope env = do
        ref <- initUnique
        u <- readIORef $ glbUnique env
        return env{glbUnique = ref, glbUniqueStack = u : glbUniqueStack env}

endUniqueScope :: Global -> IO Global
endUniqueScope env = case glbUniqueStack env of
        [] -> do
                u <- initUnique
                return env{glbUnique = u}
        hd : tl -> do
                ref <- newIORef hd
                return env{glbUnique = ref, glbUniqueStack = tl}
