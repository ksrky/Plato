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

unLoc :: Located a -> a
unLoc (L _ x) = x

noLoc :: a -> Located a
noLoc = L NoSpan

cL :: Located a -> b -> Located b
cL loc = L (getSpan loc)

cSL :: Span -> Located a -> b -> Located b
cSL sp loc = L (combineSpans sp (getSpan loc))

cLL :: Located a -> Located b -> c -> Located c
cLL loc1 loc2 = L (combineSpans (getSpan loc1) (getSpan loc2))

cSLn :: Span -> [Located a] -> b -> Located b
cSLn sp locs = L (combineSpans sp (concatSpans $ map getSpan locs))

cLnL :: [Located a] -> Located b -> c -> Located c
cLnL locs loc = L (concatSpans (map getSpan locs ++ [getSpan loc]))

cLLn :: Located a -> [Located b] -> c -> Located c
cLLn loc locs = L (concatSpans (getSpan loc : map getSpan locs))