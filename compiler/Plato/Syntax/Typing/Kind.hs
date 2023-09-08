module Plato.Syntax.Typing.Kind (
        Kind (..),
        MetaKv (..),
) where

import Data.IORef (IORef)

import Plato.Common.Pretty
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
        show (MetaKv u _) = show u

instance Ord MetaKv where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty MetaKv where
        pretty (MetaKv u _) = dollar <> pretty u

instance Pretty Kind where
        pretty = pretty' 0

instance PrettyWithContext Kind where
        pretty' _ StarK = asterisk
        pretty' p (ArrK kn1 kn2) = parenswPrec p 0 $ hsep [pretty' 0 kn1, arrow, pretty' 1 kn2]
        pretty' _ (MetaK kv) = pretty kv