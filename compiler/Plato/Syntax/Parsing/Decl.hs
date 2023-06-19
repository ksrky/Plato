{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

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
type LTopDecl = LDecl

data Decl
        = DataD Ident [Ident] [(Ident, LType)]
        | FunSpecD Ident LType
        | FunBindD Ident [Clause]
        | FixityD Ident Fixity
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
        pretty (FunSpecD id ty) = hsep [pretty id, colon, pretty ty]
        pretty (FunBindD id clauses) =
                hsep
                        [ pretty id
                        , "where"
                        , braces $ concatWith (surround $ semi <> space) (map prClause clauses)
                        ]
        pretty (FixityD id (Fixity prec dir)) = hsep [pretty dir, pretty prec, pretty id]
