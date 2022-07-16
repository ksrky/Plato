module Plato.Common.Info where

----------------------------------------------------------------
-- Info
----------------------------------------------------------------
newtype Info = Info {posInfo :: Pos} deriving (Eq)

dummyInfo :: Info
dummyInfo = Info{posInfo = dummyPos}

instance Show Info where
    show Info{posInfo = Pos 0 0} = "dummyInfo"
    show fi = show fi

----------------------------------------------------------------
-- Pos
----------------------------------------------------------------
data Pos = Pos {line :: Int, col :: Int} deriving (Eq)

dummyPos :: Pos
dummyPos = Pos{line = 0, col = 0}

instance Show Pos where
    show (Pos 0 0) = "dummyPos"
    show (Pos l c) = show l ++ ":" ++ show c
