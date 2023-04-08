module Plato.Syntax.Typing.Kind where

import Data.IORef (IORef)
import Prettyprinter

import Plato.Typing.Subst
import Plato.Common.Global

data Kind
        = StarK
        | ArrK Kind Kind
        | MetaK MetaKv
        deriving (Eq, Show)

data MetaKv = MetaKv Unique (IORef (Maybe Kind))

instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "$" ++ show u

instance Ord MetaKv where
        MetaKv u1 _ `compare` MetaKv u2 _ = u1 `compare` u2

instance Substitutable Kind where
        subst _ = id

instance Pretty Kind where
        pretty StarK = "*"
        pretty (ArrK k1 k2) = pprkind k1 <+> pretty k2
        pretty (MetaK m) = viaShow m

pprkind :: Kind -> Doc ann
pprkind StarK = pretty StarK
pprkind kn = parens (pretty kn)
