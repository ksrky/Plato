module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Plato.Common.Ident
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------

data Bind
        = -- ValBind Ident LExpr |
          FunBind Ident [Clause]
        | TypBind Ident LType
        | DatBind Ident [Quant] [(Ident, LType)]
        deriving (Eq, Show)

-- \| TypeBind Ident (Maybe Kind) LType

data Spec
        = ValSpec Ident LType
        | TypSpec Ident Kind
        deriving (Eq, Show)

data Decl
        = BindDecl Bind
        | SpecDecl Spec
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Bind
instance Pretty Spec
instance Pretty Decl