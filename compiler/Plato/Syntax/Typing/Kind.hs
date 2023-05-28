module Plato.Syntax.Typing.Kind (
        Kind (..),
        MetaKv (..),
) where

import Data.IORef (IORef)
import Prettyprinter

import Plato.Common.Uniq

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
data Kind
        = StarK
        | ArrK Kind Kind
        | MetaK MetaKv
        deriving (Eq, Show)

data MetaKv = MetaKv Uniq (IORef (Maybe Kind))

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "MetaKv " ++ show u

instance Ord MetaKv where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty MetaKv where
        pretty (MetaKv u _) = "$" <> pretty u

instance Pretty Kind where
        pretty StarK = "*"
        pretty (ArrK kn1 kn2) = prettyKind1 kn1 <+> pretty kn2
        pretty (MetaK kv) = pretty kv

prettyKind1 :: Kind -> Doc ann
prettyKind1 StarK = pretty StarK
prettyKind1 kn = parens (pretty kn)