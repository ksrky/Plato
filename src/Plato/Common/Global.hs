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
        getUnique :: MonadIO m => a -> m Unique

instance HasUnique (IORef Unique) where
        getUnique ref = liftIO $ readIORef ref

instance HasUnique Global where
        getUnique = liftIO . readIORef . glbUnique

initUnique :: IO (IORef Unique)
initUnique = newIORef 0

newUniqueScope :: Global -> IO Global
newUniqueScope glb = do
        ref <- initUnique
        u <- readIORef $ glbUnique glb
        return glb{glbUnique = ref, glbUniqueStack = u : glbUniqueStack glb}

endUniqueScope :: Global -> IO Global
endUniqueScope glb = case glbUniqueStack glb of
        [] -> do
                u <- initUnique
                return glb{glbUnique = u}
        hd : tl -> do
                ref <- newIORef hd
                return glb{glbUnique = ref, glbUniqueStack = tl}
