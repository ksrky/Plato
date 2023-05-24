module Plato.Syntax.Typing.Kind where

import Data.IORef
import Prettyprinter

import Plato.Common.Uniq 

-----------------------------------------------------------
-- Data and types
-----------------------------------------------------------
data Kind
        = MetaK MetaKv
        | StarK
        | ArrK Kind Kind
        deriving (Eq, Show)

data MetaKv = MetaKv Uniq KnRef

type KnRef = IORef (Maybe Kind)

-----------------------------------------------------------
-- Basic instances
-----------------------------------------------------------
instance Eq MetaKv where
        (MetaKv u1 _) == (MetaKv u2 _) = u1 == u2

instance Show MetaKv where
        show (MetaKv u _) = "$" ++ show u

-----------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------
instance Pretty Kind where
        pretty StarK = "*"
        pretty (ArrK k1 k2) = pprkind k1 <+> pretty k2
        pretty (MetaK m) = viaShow m

pprkind :: Kind -> Doc ann
pprkind StarK = pretty StarK
pprkind kn = parens (pretty kn)