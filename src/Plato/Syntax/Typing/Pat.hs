module Plato.Syntax.Typing.Pat where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Name.Global
import Plato.Syntax.Typing.Base

type LPat = Located Pat

data Pat
        = ConP GlbName [Pat]
        | VarP LName
        | WildP
        deriving (Eq, Show)

-----------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = pretty con <+> hsep (map pprAtomPat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprAtomPat :: Pat -> Doc ann
pprAtomPat pat@(ConP con pats)
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprAtomPat pat = pretty pat