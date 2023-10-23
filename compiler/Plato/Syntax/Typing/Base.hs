{-# LANGUAGE DerivingStrategies #-}

module Plato.Syntax.Typing.Base where

import Plato.Common.Location
import Plato.Common.Pretty

data TcFlag = Typed | Untyped

data Block a
    = Mutrec [a]
    | Nonrec a
    deriving (Eq, Foldable, Functor, Show, Traversable)

instance (HasLoc a) => HasLoc (Block a) where
    getLoc (Mutrec bnds) = getLoc bnds
    getLoc (Nonrec bnd)  = getLoc bnd

instance (Pretty a) => Pretty (Block a) where
    pretty (Mutrec bnds) = vsep $ map pretty bnds
    pretty (Nonrec bnd)  = pretty bnd
