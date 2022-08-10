module Plato.Common.Info where

data Info = Info {line :: Int, col :: Int} deriving (Eq)

dummyInfo :: Info
dummyInfo = Info{line = 0, col = 0}

instance Show Info where
    show (Info 0 0) = "dummyInfo"
    show (Info l c) = show l ++ ":" ++ show c
