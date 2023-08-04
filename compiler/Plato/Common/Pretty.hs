module Plato.Common.Pretty (module Prettyprinter, sepBy, contextParens) where

import Prettyprinter

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

contextParens :: Int -> Int -> Doc ann -> Doc ann
contextParens i j doc = if i >= j then parens doc else doc
