module Plato.Common.Pretty (
        module Prettyprinter,
        sepBy,
        contextParens,
        prettyPrint,
        PrettyWithContext (..),
        wildcard,
        arrow,
        dollar,
        asterisk,
) where

import Prettyprinter
import Prettyprinter.Render.Text

import Plato.Common.Location

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

contextParens :: Int -> Int -> Doc ann -> Doc ann
contextParens i j doc = if i > j then parens doc else doc

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putDoc . pretty

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