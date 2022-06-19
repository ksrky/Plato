module Plato.Common.Position where

data Pos = Pos {line :: Int, col :: Int} deriving (Eq)

instance Show Pos where
    show (Pos l c) = show l ++ ":" ++ show c