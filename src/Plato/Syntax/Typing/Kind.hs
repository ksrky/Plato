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
        show (MetaKv u _) = "$" ++ show u

instance Ord MetaKv where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Kind where
        pretty StarK = "*"
        pretty (ArrK k1 k2) = prettyAtom k1 <+> pretty k2
        pretty (MetaK m) = viaShow m

prettyAtom :: Kind -> Doc ann
prettyAtom StarK = pretty StarK
prettyAtom kn = parens (pretty kn)