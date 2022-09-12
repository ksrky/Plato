{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Plato.Common.SrcLoc where

import Plato.Common.Pretty

data Loc
        = Loc
                !Int -- line number
                !Int -- column number
        deriving (Eq, Ord, Show)

instance Pretty Loc where
        pretty (Loc l c) = show l ++ ":" ++ show c

data Span
        = Span
                Loc -- start loc
                Loc -- end loc
        | NoSpan
        deriving (Eq, Ord, Show)

instance Pretty Span where
        pretty (Span s e) = pretty s ++ "-" ++ pretty e
        pretty NoSpan = ""

combineSpans :: Span -> Span -> Span
combineSpans (Span s1 e1) (Span s2 e2) = Span (s1 `min` s2) (e1 `max` e2)
combineSpans (Span s1 e1) NoSpan = Span s1 e1
combineSpans NoSpan (Span s2 e2) = Span s2 e2
combineSpans NoSpan NoSpan = NoSpan

concatSpans :: [Span] -> Span
concatSpans [] = NoSpan
concatSpans [sp] = sp
concatSpans (sp : sps) = combineSpans sp (concatSpans sps)

data Located a = L Span a
        deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

getSpan :: Located a -> Span
getSpan (L sp _) = sp
