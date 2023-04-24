module Plato.Syntax.Parsing.Pat where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path

----------------------------------------------------------------
-- Data and types
----------------------------------------------------------------
type LPat = Located Pat

data Pat
        = ConP Path [LPat]
        | VarP Ident
        | WildP
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Pat where
        substPath (ConP path pats) = ConP <$> substPath path <*> mapM (traverse substPath) pats
        substPath (VarP id) = return $ VarP id
        substPath WildP = return WildP

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