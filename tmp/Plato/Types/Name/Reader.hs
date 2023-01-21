module Plato.Types.Name.Reader where

import Plato.Types.Name
import Prettyprinter

data RdrName
        = Unqual Name
        | Qual ModuleName Name
        deriving (Eq, Show)

instance Pretty RdrName where
        pretty (Unqual n) = pretty n
        pretty (Qual modn n) = hcat [pretty modn, dot, pretty n]