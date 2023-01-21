module Plato.Common.Fixity where

import Data.Map.Strict as M
import Prettyprinter

----------------------------------------------------------------
-- Precedence
----------------------------------------------------------------
type FixPrec = Int

maxPrec :: Int
maxPrec = 9

minPrec :: Int
minPrec = 0

----------------------------------------------------------------
-- Fixity
----------------------------------------------------------------
data FixDir = Leftfix | Rightfix | Nonfix deriving (Eq, Show)

instance Pretty FixDir where
        pretty Leftfix = "infixl"
        pretty Rightfix = "infixr"
        pretty Nonfix = "infix"

data Fixity = Fixity FixPrec FixDir deriving (Eq, Show)

----------------------------------------------------------------
-- FixityEnv
----------------------------------------------------------------
type FixityEnv a = M.Map a Fixity