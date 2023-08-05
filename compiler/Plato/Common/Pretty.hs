module Plato.Common.Pretty (
        module Prettyprinter,
        sepBy,
        contextParens,
        prettyPrint,
        PrettyWithContext (..),
) where

import Prettyprinter
import Prettyprinter.Render.Text

sepBy :: [Doc ann] -> Doc ann -> Doc ann
sepBy doc sep = concatWith (surround (sep <> space)) doc

contextParens :: Int -> Int -> Doc ann -> Doc ann
contextParens i j doc = if i > j then parens doc else doc

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putDoc . pretty

class PrettyWithContext a where
        pretty' :: Int -> a -> Doc ann