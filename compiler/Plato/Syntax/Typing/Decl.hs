{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Decl where

import Data.Foldable
import Data.Graph

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------

data Bind (a :: TcFlag) where
        Bind :: (Ident, Maybe LType) -> LExpr 'Untyped -> Bind 'Untyped
        Bind' :: (Ident, Type) -> Expr 'Typed -> Bind 'Typed

data TypDefn (a :: TcFlag) where
        DatDefn :: Ident -> Quants -> [(Ident, LType)] -> TypDefn 'Untyped
        DatDefn' :: (Ident, Kind) -> Quants -> [(Ident, LType)] -> TypDefn 'Typed

data Defn (a :: TcFlag)
        = ValDefn (SCC (Bind a))
        | TypDefn (SCC (TypDefn a))

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Bind a)
deriving instance Show (Bind a)
deriving instance Eq (TypDefn a)
deriving instance Show (TypDefn a)
deriving instance Eq (Defn a)
deriving instance Show (Defn a)

instance HasLoc (Bind 'Untyped) where
        getLoc (Bind (id, _) c) = getLoc id <> getLoc c

instance HasLoc (TypDefn 'Untyped) where
        getLoc (DatDefn id _ ctors) = getLoc id <> mconcat (map getLoc ctors)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn a) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs

instance Pretty (Bind a) where
        pretty (Bind (id, Just ty) exp) = hsep [pretty id, colon, pretty ty, "where", pretty exp]
        pretty (Bind (id, Nothing) exp) = hsep [pretty id, "where", pretty exp]
        pretty (Bind' (id, ty) exp) = hsep [pretty id, colon, pretty ty, equals, pretty exp]

instance Pretty (TypDefn a) where
        pretty (DatDefn id params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $ map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs `sepBy` semi
                        ]
        pretty (DatDefn' (id, kn) params constrs) =
                hsep
                        [ "data"
                        , pretty id
                        , semi
                        , hsep $ pretty kn : map (parens . prQuant) params
                        , "where"
                        , braces $ map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs `sepBy` semi
                        ]

instance Pretty (SCC (Bind a)) where
        pretty (AcyclicSCC bnd) = pretty bnd
        pretty (CyclicSCC bnds) = braces $ map pretty bnds `sepBy` semi