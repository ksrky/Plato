module Plato.Common.Name.Reader where

import Plato.Common.Name
import Prettyprinter

data RdrName
        = Unqual Name
        | Qual ModuleName Name
        deriving (Eq, Show)

instance Pretty RdrName where
        pretty (Unqual n) = pretty n
        pretty (Qual modn n) = hcat [pretty modn, dot, pretty n]