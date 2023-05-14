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

data Expr
        = VarE Ident
        | AppE LExpr LExpr
        | LamE [LPat] LExpr
        | LetE [LFunDecl] LExpr
        deriving (Eq, Show)

-- newtype Op = Op Path deriving (Eq, Show)

data FunDecl
        = FunBind Ident [LPat] LExpr
        | FunSpec Ident LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr

instance Pretty FunDecl