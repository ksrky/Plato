{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Plato.Common.Location where

import Prettyprinter

data Loc
        = Loc
                !FilePath -- file name
                !Int -- line number
                !Int -- column number
        deriving (Eq, Ord, Show)

instance Pretty Loc where
        pretty (Loc f l c) = hcat [pretty f, colon, pretty l, colon, pretty c]

data Span
        = Span
                Loc -- start loc
                Loc -- end loc
        | NoSpan
        deriving (Eq, Ord, Show)

instance Pretty Span where
        pretty (Span s (Loc _ el ec)) = hcat [pretty s, "-", pretty el, colon, pretty ec]
        pretty NoSpan = emptyDoc

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

instance Pretty a => Pretty (Located a) where
        pretty (L _ x) = pretty x

getLoc :: Located a -> Span
getLoc (L sp _) = sp

unLoc :: Located a -> a
unLoc (L _ x) = x

noLoc :: a -> Located a
noLoc = L NoSpan

cL :: Located a -> b -> Located b
cL loc = L (getLoc loc)

cSL :: Span -> Located a -> b -> Located b
cSL sp loc = L (combineSpans sp (getLoc loc))

cLL :: Located a -> Located b -> c -> Located c
cLL loc1 loc2 = L (combineSpans (getLoc loc1) (getLoc loc2))

cSLn :: Span -> [Located a] -> b -> Located b
cSLn sp locs = L (combineSpans sp (concatSpans $ map getLoc locs))

cLnL :: [Located a] -> Located b -> c -> Located c
cLnL locs loc = L (concatSpans (map getLoc locs ++ [getLoc loc]))

cLLn :: Located a -> [Located b] -> c -> Located c
cLLn loc locs = L (concatSpans (getLoc loc : map getLoc locs))