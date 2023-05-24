module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Plato.Common.Location
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Type

type LDecl = Located Decl

data Decl
        = TypeD LName LType
        | VarD LName LType
        | ConD FuncD
        deriving (Eq, Show)

instance Pretty Decl where
        pretty (TypeD con body) = hsep [pretty con, equals, pretty body]
        pretty (VarD var ty) = hsep [pretty var, colon, pretty ty]
        pretty (ConD fund) = pretty fund
