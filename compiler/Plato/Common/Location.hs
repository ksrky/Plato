module Plato.Common.Location where

import Prettyprinter

-- | Source location includes file name, line number and column number
data Loc
        = Loc
                !FilePath -- file name
                !Int -- line number
                !Int -- column number
        deriving (Eq, Ord)

instance Show Loc where
        show (Loc f l c) = show f ++ ":" ++ show l ++ ":" ++ show c
instance Pretty Loc where
        pretty (Loc f l c) = hcat [pretty f, colon, pretty l, colon, pretty c]

-- | From start location to end location
data Span
        = Span
                !Loc -- start loc
                !Loc -- end loc
        | NoSpan
        deriving (Eq, Ord)

instance Show Span where
        show NoSpan = "NoSpan"
        show (Span s (Loc _ l c)) = show s ++ "-" ++ show l ++ ":" ++ show c

instance Pretty Span where
        pretty (Span s (Loc _ l c)) = hcat [pretty s, "-", pretty l, colon, pretty c]
        pretty NoSpan = emptyDoc

-- | Combining spans
combineSpans :: Span -> Span -> Span
combineSpans (Span s1 e1) (Span s2 e2) = Span (s1 `min` s2) (e1 `max` e2)
combineSpans (Span s1 e1) NoSpan = Span s1 e1
combineSpans NoSpan (Span s2 e2) = Span s2 e2
combineSpans NoSpan NoSpan = NoSpan

concatSpans :: [Span] -> Span
concatSpans [] = NoSpan
concatSpans [sp] = sp
concatSpans (sp : sps) = combineSpans sp (concatSpans sps)

data Located a = L {_span :: !Span, unLoc :: a}
        deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

noLoc :: a -> Located a
noLoc = L NoSpan

class HasLoc a where
        getLoc :: a -> Span

instance HasLoc Span where
        getLoc = id

instance HasLoc (Located a) where
        getLoc (L sp _) = sp

instance HasLoc a => HasLoc [a] where
        getLoc locs = concatSpans (map getLoc locs)

sL :: (HasLoc a, HasLoc b) => a -> b -> c -> Located c
sL x y = L (combineSpans (getLoc x) (getLoc y))

instance Pretty a => Pretty (Located a) where
        pretty (L _ x) = pretty x
