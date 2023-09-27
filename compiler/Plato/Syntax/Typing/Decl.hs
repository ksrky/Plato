{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

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
type family AnnotKn (a :: TcFlag) where
        AnnotKn 'Untyped = ()
        AnnotKn 'Typed = Kind

data Bind (a :: TcFlag) = Bind (Ident, Annot a) (XExpr a)

data TypDefn (a :: TcFlag) = DatDefn Ident (AnnotKn a) Quants [(Ident, LType)]

data Defn (a :: TcFlag)
        = ValDefn (SCC (Bind a))
        | TypDefn (SCC (TypDefn a))

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Bind 'Untyped)
deriving instance Eq (Bind 'Typed)
deriving instance Show (Bind 'Untyped)
deriving instance Show (Bind 'Typed)
deriving instance Eq (TypDefn 'Untyped)
deriving instance Eq (TypDefn 'Typed)
deriving instance Show (TypDefn 'Untyped)
deriving instance Show (TypDefn 'Typed)
deriving instance Eq (Defn 'Untyped)
deriving instance Eq (Defn 'Typed)
deriving instance Show (Defn 'Untyped)
deriving instance Show (Defn 'Typed)

instance HasLoc (Bind 'Untyped) where
        getLoc (Bind (id, _) c) = getLoc id <> getLoc c

instance HasLoc (TypDefn 'Untyped) where
        getLoc (DatDefn id _ _ ctors) = getLoc id <> mconcat (map getLoc ctors)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn 'Untyped) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs

instance Pretty (Defn 'Typed) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs

instance Pretty (Bind 'Untyped) where
        pretty (Bind (id, Just ty) exp) = hsep [pretty id, colon, pretty ty, "where", pretty exp]
        pretty (Bind (id, Nothing) exp) = hsep [pretty id, "where", pretty exp]

instance Pretty (Bind 'Typed) where
        pretty (Bind (id, ty) exp) = hsep [pretty id, colon, pretty ty, "where", pretty exp]

instance Pretty (TypDefn a) where
        pretty (DatDefn id _ params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $ map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs `sepBy` semi
                        ]

instance Pretty (SCC (Bind 'Untyped)) where
        pretty (AcyclicSCC bnd) = pretty bnd
        pretty (CyclicSCC bnds) = braces $ map pretty bnds `sepBy` semi

instance Pretty (SCC (Bind 'Typed)) where
        pretty (AcyclicSCC bnd) = pretty bnd
        pretty (CyclicSCC bnds) = braces $ map pretty bnds `sepBy` semi