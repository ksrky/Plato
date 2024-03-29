module Plato.Syntax.Parsing.Pat (
        LPat,
        Pat (..),
        prAtomPat,
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Data and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Ident [LPat]
        | VarP Ident
        | WildP
        | BinP LPat Ident LPat
        | AnnP LPat LType
        | FactorP LPat
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = hsep (pretty con : map prAtomPat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"
        pretty (BinP lhs op rhs) = hsep [prAtomPat lhs, pretty op, prAtomPat rhs]
        pretty (AnnP pat ann_ty) = parens $ hsep [pretty pat, pretty ann_ty]
        pretty (FactorP pat) = parens $ pretty pat

prAtomPat :: LPat -> Doc ann
prAtomPat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
prAtomPat pat@(L _ BinP{}) = parens $ pretty pat
prAtomPat pat@(L _ FactorP{}) = parens $ pretty pat
prAtomPat pat = pretty pat