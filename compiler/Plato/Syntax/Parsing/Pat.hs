module Plato.Syntax.Parsing.Pat (
        LPat,
        Pat (..),
        prAtomPat,
) where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location

----------------------------------------------------------------
-- Data and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Ident [LPat]
        | VarP Ident
        | WildP
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = hsep (pretty con : map prAtomPat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

prAtomPat :: LPat -> Doc ann
prAtomPat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
prAtomPat pat = pretty pat