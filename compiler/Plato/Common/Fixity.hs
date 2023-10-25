module Plato.Common.Fixity where

import Plato.Common.Pretty

newtype FixPrec
    = FixPrec Int
    deriving (Eq, Ord)

instance Show FixPrec where
    show (FixPrec n) = show n

instance Bounded FixPrec where
    minBound = FixPrec 0
    maxBound = FixPrec 9

instance Enum FixPrec where
    fromEnum (FixPrec n) = n
    toEnum = FixPrec

data FixDir = Prefix | Leftfix | Rightfix | Nonfix
    deriving (Eq, Show)

data Fixity = Fixity FixPrec FixDir
    deriving (Eq, Show)

identFixity :: Fixity
identFixity = Fixity maxBound Prefix

operFixity :: Fixity
operFixity = Fixity maxBound Leftfix

instance Pretty FixPrec where
    pretty (FixPrec n) = pretty n

instance Pretty FixDir where
    pretty Prefix   = "prefix"
    pretty Leftfix  = "infixl"
    pretty Rightfix = "infixr"
    pretty Nonfix   = "infix"
