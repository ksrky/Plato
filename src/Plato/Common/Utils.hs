{-# LANGUAGE RankNTypes #-}

module Plato.Common.Utils where

import Control.Monad.State

-- | Since base-4.18.0.0
mapAccumM :: (Monad m, Traversable t) => (s -> a -> m (b, s)) -> s -> t a -> m (t b, s)
mapAccumM f s t = runStateT (mapM (StateT . flip f) t) s

-- | Since base-4.18.0.0
forAccumM :: (Monad m, Traversable t) => s -> t a -> (s -> a -> m (b, s)) -> m (t b, s)
forAccumM s t f = mapAccumM f s t

safeAt :: [a] -> Int -> Maybe a
safeAt xs i =
        if i < length xs
                then Just (xs !! i)
                else Nothing