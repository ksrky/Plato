module Plato.Syntax.Abstract.Pat where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Ident
import Plato.Common.Path

type LPat = Located Pat

data Pat
        = ConP Path [LPat]
        | VarP Ident
        | WildP
        deriving (Eq, Show)

instance Pretty Pat where
        pretty (ConP con pats) = pretty con <+> hsep (map (pprAtomPat . unLoc) pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprAtomPat :: Pat -> Doc ann
pprAtomPat pat@(ConP con pats)
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprAtomPat pat = pretty pat
