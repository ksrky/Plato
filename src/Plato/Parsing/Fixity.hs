module Plato.Parsing.Fixity where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Common.Table

data Fixity = Leftfix | Rightfix | Nonfix
        deriving (Eq, Show)

data Op = Op (Located Name) Int Fixity
        deriving (Eq, Show)

type OpDict = Table Op

maxPrec :: Int
maxPrec = 9

minPrec :: Int
minPrec = 0