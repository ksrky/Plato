{-# OPTIONS_GHC -Wno-partial-fields #-}

module Plato.Syntax.Parsing.Decl where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LTopDecl = Located TopDecl

data TopDecl
        = DataD Ident [Ident] [(Ident, LType)]
        | LocalD {unLocalD :: LocDecl}
        deriving (Eq, Show)

instance Ord TopDecl where
        compare x y = compare (tag x) (tag y)
            where
                tag :: TopDecl -> Int
                tag DataD{} = 0
                tag LocalD{} = 1

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty TopDecl where
        pretty (DataD con args constrs) =
                hsep
                        [ "data"
                        , hsep (pretty con : map pretty args)
                        , "where"
                        , braces $ map (\(id, ty) -> hsep [pretty id, colon, pretty ty]) constrs `sepBy` semi
                        ]
        pretty (LocalD ld) = pretty ld
