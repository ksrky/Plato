module Plato.Common.Pretty (
        module Prettyprinter,
        Pretty (..),
        sepBy,
        parenswPrec,
        printList,
        wildcard,
        arrow,
        dollar,
        asterisk,
) where

import Prettyprinter hiding (Pretty (..))
import Prettyprinter qualified

import Prettyprinter.Util

class Pretty a where
        pretty :: a -> Doc ann
        pretty = pretty' 0
        pretty' :: Int -> a -> Doc ann
        pretty' _ = pretty

instance Pretty String where
        pretty = Prettyprinter.pretty

instance Pretty Int where
        pretty = Prettyprinter.pretty

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

parenswPrec :: Int -> Int -> Doc ann -> Doc ann
parenswPrec i j doc = if i > j then parens doc else doc

printList :: (Pretty a) => [a] -> IO ()
printList = putDocW 100 . (<> line) . vsep . map pretty

wildcard :: Doc ann
wildcard = "_"

arrow :: Doc ann
arrow = "->"

dollar :: Doc ann
dollar = "$"

asterisk :: Doc ann
asterisk = "*"