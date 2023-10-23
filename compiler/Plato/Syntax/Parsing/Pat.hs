module Plato.Syntax.Parsing.Pat where

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
    pretty' _ (ConP con [])     = pretty con
    pretty' p (ConP con pats)   = parenswPrec p 0 $ hsep (pretty con : map (pretty' 1) pats)
    pretty' _ (VarP var)        = pretty var
    pretty' _ WildP             = wildcard
    pretty' _ (BinP lhs op rhs) = parens $ hsep [pretty' 1 lhs, pretty op, pretty' 1 rhs]
    pretty' _ (AnnP pat ann_ty) = parens $ hsep [pretty pat, pretty ann_ty]
    pretty' _ (FactorP pat)     = parens $ pretty pat
