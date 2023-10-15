{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Plato.Syntax.Typing.Decl where

import Data.Foldable

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type XValDefns (a :: TcFlag) = XBinds a

data TypDefn (a :: TcFlag) = DatDefn Ident Quants [(Ident, LType)]

type family XTypDefn (a :: TcFlag) where
        XTypDefn 'Untyped = Located (TypDefn 'Untyped)
        XTypDefn 'Typed = TypDefn 'Typed

type XTypDefns (a :: TcFlag) = Block (XTypDefn a)

data Defn (a :: TcFlag)
        = ValDefn (XValDefns a)
        | TypDefn (XTypDefns a)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
deriving instance Eq (TypDefn 'Untyped)
deriving instance Eq (TypDefn 'Typed)
deriving instance Show (TypDefn 'Untyped)
deriving instance Show (TypDefn 'Typed)
deriving instance Eq (Defn 'Untyped)
deriving instance Eq (Defn 'Typed)
deriving instance Show (Defn 'Untyped)
deriving instance Show (Defn 'Typed)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Defn 'Untyped) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs

instance Pretty (Defn 'Typed) where
        pretty (ValDefn binds) = indent 2 $ vsep $ toList $ fmap pretty binds
        pretty (TypDefn tdefs) = indent 2 $ vsep $ toList $ fmap pretty tdefs

instance Pretty (TypDefn a) where
        pretty (DatDefn id params constrs) =
                hsep
                        [ "data"
                        , hsep (pretty id : map (parens . prQuant) params)
                        , "where"
                        , braces $ map (\(con, ty) -> hsep [pretty con, colon, pretty ty]) constrs `sepBy` semi
                        ]