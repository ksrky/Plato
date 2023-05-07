module Plato.Syntax.Parsing.TopDecl where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr

type LTopDecl = Located TopDecl

data TopDecl
        = Import Path
        | Decl LDecl
        | Eval LExpr
        deriving (Eq, Show)

instance Pretty TopDecl where
        pretty (Import path) = hsep ["import", pretty path]
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp
