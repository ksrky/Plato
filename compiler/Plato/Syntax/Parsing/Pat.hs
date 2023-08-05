module Plato.Syntax.Parsing.Pat (
        LPat,
        Pat (..),
        prAtomPat,
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty

----------------------------------------------------------------
-- Data and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Ident [LPat]
        | VarP Ident
        | WildP
        | InfixP LPat Ident LPat
        | FactorP LPat
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = hsep (pretty con : map prAtomPat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"
        pretty (InfixP lhs op rhs) = hsep [prAtomPat lhs, pretty op, prAtomPat rhs]
        pretty (FactorP pat) = parens $ pretty pat

prAtomPat :: LPat -> Doc ann
prAtomPat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
prAtomPat pat@(L _ InfixP{}) = parens $ pretty pat
prAtomPat pat@(L _ FactorP{}) = parens $ pretty pat
prAtomPat pat = pretty pat