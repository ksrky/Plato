module Plato.Syntax.Parsing.TopDecl where

import Prettyprinter

import Plato.Common.Location
import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr

type LTopDecl = Located TopDecl

data TopDecl
        = Decl LDecl
        | Eval LExpr
        deriving (Eq, Show)

instance Pretty TopDecl where
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp
