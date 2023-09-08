module Plato.Common.Pretty (
        module Prettyprinter,
        sepBy,
        parenswPrec,
        printList,
        PrettyWithContext (..),
        wildcard,
        arrow,
        dollar,
        asterisk,
) where

import Prettyprinter

import Plato.Common.Location
import Prettyprinter.Util

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

parenswPrec :: Int -> Int -> Doc ann -> Doc ann
parenswPrec i j doc = if i > j then parens doc else doc

printList :: Pretty a => [a] -> IO ()
printList = putDocW 100 . (<> line) . vsep . map pretty

class PrettyWithContext a where
        pretty' :: Int -> a -> Doc ann

instance PrettyWithContext a => PrettyWithContext (Located a) where
        pretty' c (L _ x) = pretty' c x

wildcard :: Doc ann
wildcard = "_"

arrow :: Doc ann
arrow = "->"

dollar :: Doc ann
dollar = "$"

asterisk :: Doc ann
asterisk = "*"