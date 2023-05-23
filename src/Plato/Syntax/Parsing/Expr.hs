{-# LANGUAGE FlexibleInstances #-}

module Plato.Syntax.Parsing.Expr where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr
type LFunDecl = Located FunDecl

type Clause = ([LPat], LExpr)

data Expr
        = VarE Ident
        | AppE LExpr LExpr
        | OpE LExpr Ident LExpr
        | LamE [LPat] LExpr
        | LetE [LFunDecl] LExpr
        | FactorE LExpr
        deriving (Eq, Show)

data FunDecl
        = FunBind Ident [Clause]
        | FunSpec Ident LType
        | FixDecl Ident Fixity
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
        pretty (OpE lhs op rhs) = parens $ pretty lhs <+> pretty op <+> pretty rhs
        pretty (LamE vars body) = hsep [backslash <> hsep (map pretty vars), "->", pretty body]
        pretty (LetE decs body) =
                hsep ["let", lbrace, concatWith (surround semi) (map pretty decs), rbrace, "in", pretty body]
        pretty (FactorE exp) = pretty exp

prExpr2 :: Expr -> Doc ann
prExpr2 e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = prExpr1 e' <+> sep (map prExpr1 es)

prExpr1 :: Expr -> Doc ann
prExpr1 e@VarE{} = pretty e
prExpr1 e = parens (pretty e)

instance Pretty FunDecl

instance Pretty FixDir where
        pretty Leftfix = "infixl"
        pretty Rightfix = "infixr"
        pretty Nonfix = "infix"