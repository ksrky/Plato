module Plato.Syntax.Typing.Base where

import Data.Graph
import Prettyprinter

import Plato.Common.Location

data TcFlag = Typed | Untyped

newtype RecBlock a = RecBlock (SCC a)
        deriving (Eq, Show, Foldable, Functor, Traversable)

mutrec :: [a] -> RecBlock a
mutrec = RecBlock . CyclicSCC

nonrec :: a -> RecBlock a
nonrec = RecBlock . AcyclicSCC

instance (HasLoc a) => HasLoc (RecBlock a) where
        getLoc (RecBlock scc) = getLoc scc

instance (Pretty a) => Pretty (RecBlock a) where
        pretty (RecBlock scc) = vsep $ map pretty $ flattenSCC scc