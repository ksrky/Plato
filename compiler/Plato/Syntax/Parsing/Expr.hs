{-# LANGUAGE FlexibleInstances #-}

module Plato.Syntax.Parsing.Expr where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import {-# SOURCE #-} Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Pat

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr

type Clause = ([LPat], LExpr)

data Expr
        = VarE Ident
        | AppE LExpr LExpr
        | BinE LExpr Ident LExpr
        | LamE [LPat] LExpr
        | LetE [LDecl] LExpr
        | CaseE LExpr [(LPat, LExpr)]
        | FactorE LExpr
        deriving (Eq, Show)

type FixPrec = Int
data FixDir = Leftfix | Rightfix | Nonfix deriving (Eq, Show)

data Fixity = Fixity FixPrec FixDir deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance HasLoc Clause where
        getLoc (pats, exp) = combineSpans (getLoc pats) (getLoc exp)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = prExpr2 exp
        pretty (BinE lhs op rhs) = parens $ pretty lhs <+> pretty op <+> pretty rhs
        pretty (LamE vars body) = hsep [backslash <> hsep (map pretty vars), "->", pretty body]
        pretty (LetE decs body) =
                hsep ["let", braces $ map pretty decs `sepBy` semi, "in", pretty body]
        pretty (CaseE match alts) =
                hsep
                        [ "case"
                        , pretty match
                        , "of"
                        , braces $ map (\(p, e) -> hsep [pretty p, "->", pretty e]) alts `sepBy` semi
                        ]
        pretty (FactorE exp) = pretty exp

prExpr2 :: Expr -> Doc ann
prExpr2 e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = prExpr1 e' <+> hsep (map prExpr1 es)

prExpr1 :: Expr -> Doc ann
prExpr1 e@VarE{} = pretty e
prExpr1 e = parens (pretty e)

prClause :: Clause -> Doc ann
prClause (pats, exp) = hsep (map prAtomPat pats ++ ["->", pretty exp])

instance Pretty FixDir where
        pretty Leftfix = "infixl"
        pretty Rightfix = "infixr"
        pretty Nonfix = "infix"