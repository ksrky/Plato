module Plato.Syntax.Parsing.TopDecl where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr

type LTopDecl = Located TopDecl

data TopDecl
        = Import Path Bool
        | Decl LDecl
        | Eval LExpr
        deriving (Eq, Show)

instance Pretty TopDecl where
        pretty (Import path isopen) =
                if isopen
                        then hsep ["open import", pretty path]
                        else hsep ["import", pretty path]
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp
