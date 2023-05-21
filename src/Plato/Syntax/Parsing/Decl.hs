module Plato.Syntax.Parsing.Decl where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LDecl = Located Decl

-- type LModule = Located Module

data Decl
        = DataD Ident [Ident] [(Ident, LType)]
        | FuncD FunDecl
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Decl