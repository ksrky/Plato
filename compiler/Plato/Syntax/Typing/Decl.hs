{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Plato.Syntax.Typing.Decl where

import Data.Foldable

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type XValDefns (a :: TcFlag) = XBinds a

data TypDefn = DatDefn Ident Quants [(Ident, LType)]
        deriving (Eq, Show)

type family XTypDefn (a :: TcFlag) where
        XTypDefn 'Untyped = Located TypDefn
        XTypDefn 'Typed = TypDefn

type XTypDefns (a :: TcFlag) = Block (XTypDefn a)

data ModDefn (a :: TcFlag)
        = ModPath Ident Path
        | ModBody Ident [Defn a]

type family XModDefn (a :: TcFlag) where
        XModDefn 'Untyped = Located (ModDefn 'Untyped)
        XModDefn 'Typed = ModDefn 'Typed

data Defn (a :: TcFlag)
        = ValDefn (XValDefns a)
        | TypDefn (XTypDefns a)
        | ModDefn (XModDefn a)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (Defn 'Untyped)
deriving instance Eq (Defn 'Typed)
deriving instance Show (Defn 'Untyped)
deriving instance Show (Defn 'Typed)
deriving instance Eq (ModDefn 'Untyped)
deriving instance Eq (ModDefn 'Typed)
deriving instance Show (ModDefn 'Untyped)
deriving instance Show (ModDefn 'Typed)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn 'Untyped) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs
        pretty (ModDefn mod) = pretty mod

instance Pretty (Defn 'Typed) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs
        pretty (ModDefn mod) = pretty mod

instance Pretty TypDefn where
        pretty (DatDefn id params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $ map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs `sepBy` semi
                        ]

instance Pretty (ModDefn 'Untyped) where
        pretty (ModPath id path) = hsep ["module", pretty id, "=", pretty path]
        pretty (ModBody id defs) =
                hsep ["module", pretty id, "where"] <> line <> indent 2 (vsep $ map pretty defs)

instance Pretty (ModDefn 'Typed) where
        pretty (ModPath id path) = hsep ["module", pretty id, "=", pretty path]
        pretty (ModBody id defs) =
                hsep ["module", pretty id, "where"] <> line <> indent 2 (vsep $ map pretty defs)