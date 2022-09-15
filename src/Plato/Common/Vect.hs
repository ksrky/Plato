module Plato.Common.Vect where

import qualified Data.Vector as V

type Vect a = V.Vector a

empty :: V.Vector a
empty = V.empty

cons :: a -> V.Vector a -> V.Vector a
cons = V.cons

uncons :: V.Vector a -> Maybe (a, V.Vector a)
uncons = V.uncons

update :: Int -> a -> V.Vector a -> V.Vector a
update i a v = v V.// [(i, a)]

toList :: V.Vector a -> [a]
toList = V.toList

(!) :: V.Vector a -> Int -> a
(!) = (V.!)

look :: Eq a => a -> Vect (a, b) -> Maybe b
look k v = do
        ((x, y), tl) <- uncons v
        if k == x then Just y else look k tl

find :: Eq a => a -> Vect (a, b) -> Maybe (Int, b)
find = find' 0
    where
        find' i k v = do
                ((x, y), tl) <- uncons v
                if k == x then Just (i, y) else find' (i + 1) k tl

elemIndex :: Eq a => a -> Vect a -> Maybe Int
elemIndex = V.elemIndex

vmap :: (a -> b) -> V.Vector a -> V.Vector b
vmap = V.map