module Plato.Syntax.Parsing.Pat where

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
-- Basic instances
----------------------------------------------------------------

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty (ConP con pats) = hsep (pretty con : map pprpat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: LPat -> Doc ann
pprpat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat