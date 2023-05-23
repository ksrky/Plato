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
instance Pretty Expr

instance Pretty FunDecl

instance Pretty FixDir where
        pretty Leftfix = "infixl"
        pretty Rightfix = "infixr"
        pretty Nonfix = "infix"