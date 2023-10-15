module Plato.Syntax.Typing.Pat (LPat, Pat (..)) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Ident [LPat]
        | VarP Ident
        | WildP
        | AnnP LPat Type
        | TagP Ident [(Ident, Type)]
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Pat where
        pretty' _ (ConP con []) = pretty con
        pretty' p (ConP con pats) = parenswPrec p 0 $ hsep (pretty con : map (pretty' 1) pats)
        pretty' _ (VarP var) = pretty var
        pretty' _ WildP = wildcard
        pretty' _ (AnnP pat ann_ty) = parens $ hsep [pretty' 0 pat, colon, pretty' 0 ann_ty]
        pretty' _ (TagP con args) = hsep $ map pretty (con : map fst args)