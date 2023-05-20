module Plato.Syntax.Parsing.TopDecl where

import Prettyprinter

import Plato.Common.Location
import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr

type LTopDecl = Located TopDecl

type TopDecl = Decl
