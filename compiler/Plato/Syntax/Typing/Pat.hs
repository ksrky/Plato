module Plato.Syntax.Typing.Pat (LPat, Pat (..), prAtomPat) where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Ident [LPat]
        | VarP Ident
        | WildP
        | TagP Ident [(Ident, Type)]
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = hsep (pretty con : map prAtomPat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"
        pretty (TagP con args) = hsep (map pretty (con : map fst args))

prAtomPat :: LPat -> Doc ann
prAtomPat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
prAtomPat pat@(L _ (TagP con args))
        | null args = pretty con
        | otherwise = parens $ pretty pat
prAtomPat pat = pretty pat