module Plato.Common.Pretty (module Plato.Common.Pretty, module Prettyprinter) where

import Prettyprinter hiding (Pretty (..))
import Prettyprinter qualified

class Pretty a where
    pretty :: a -> Doc ann
    pretty = pretty' 0
    pretty' :: Int -> a -> Doc ann
    pretty' _ = pretty
    prettyList :: [a] -> Doc ann
    prettyList = vsep . map pretty

instance Pretty String where
    pretty = Prettyprinter.pretty

instance Pretty Int where
    pretty = Prettyprinter.pretty

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

parenswPrec :: Int -> Int -> Doc ann -> Doc ann
parenswPrec i j doc = if i > j then parens doc else doc

wildcard :: Doc ann
wildcard = "_"

arrow :: Doc ann
arrow = "->"

dollar :: Doc ann
dollar = "$"

asterisk :: Doc ann
asterisk = "*"
