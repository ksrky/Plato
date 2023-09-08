{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Plato.Syntax.Typing.Decl where

import Data.Foldable

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
        Bind :: (Ident, Maybe LType) -> Clauses 'Untyped -> Bind 'Untyped
        Bind' :: (Ident, Type) -> Expr 'Typed -> Bind 'Typed

data TypDefn (a :: TcFlag) where
        DatDefn :: Ident -> Quants -> [(Ident, LType)] -> TypDefn 'Untyped
        DatDefn' :: (Ident, Kind) -> Quants -> [(Ident, LType)] -> TypDefn 'Typed

data Rec a = NonRec a | Rec [a] deriving (Eq, Show)

data Defn (a :: TcFlag)
        = ValDefn (Rec (Bind a))
        | TypDefn [TypDefn a]

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Bind a)
deriving instance Show (Bind a)
deriving instance Eq (TypDefn a)
deriving instance Show (TypDefn a)
deriving instance Eq (Defn a)
deriving instance Show (Defn a)

instance Foldable Rec where
        foldMap f (NonRec x) = f x
        foldMap f (Rec xs) = foldMap f xs

instance Functor Rec where
        fmap f (NonRec x) = NonRec $ f x
        fmap f (Rec xs) = Rec $ fmap f xs

instance Traversable Rec where
        traverse f (NonRec x) = NonRec <$> f x
        traverse f (Rec xs) = Rec <$> traverse f xs

instance HasLoc (Bind 'Untyped) where
        getLoc (Bind (id, _) c) = getLoc id <> getLoc c

instance HasLoc (TypDefn 'Untyped) where
        getLoc (DatDefn id _ ctors) = getLoc id <> mconcat (map getLoc ctors)

instance HasLoc a => HasLoc (Rec a) where
        getLoc (NonRec x) = getLoc x
        getLoc (Rec xs) = mconcat $ map getLoc xs

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn a) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ map pretty tdefs

instance Pretty (Bind a) where
        pretty (Bind (id, Just ty) clauses) =
                hsep
                        [ pretty id
                        , colon
                        , pretty ty
                        , "where"
                        , braces $ map prClause clauses `sepBy` semi
                        ]
        pretty (Bind (id, Nothing) clauses) =
                hsep
                        [ pretty id
                        , "where"
                        , braces $ map prClause clauses `sepBy` semi
                        ]
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

instance Pretty (Rec (Bind a)) where
        pretty (NonRec bnd) = pretty bnd
        pretty (Rec bnds) = braces $ map pretty bnds `sepBy` semi
