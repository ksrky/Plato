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

data Decl
        = DataD Ident [Ident] [(Ident, LType)]
        | FuncD [LFunDecl]
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Decl where
        pretty (DataD con args constrs) =
                hsep
                        [ "data"
                        , hsep (pretty con : map pretty args)
                        , "where"
                        , braces $
                                concatWith
                                        (surround $ semi <> space)
                                        (map (\(id, ty) -> hsep [pretty id, colon, pretty ty]) constrs)
                        ]
        pretty (FuncD fundecs) = concatWith (surround $ semi <> space) (map pretty fundecs)
