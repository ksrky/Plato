module Plato.Common.GenName where

import Plato.Common.Name
import Plato.Common.SrcLoc

----------------------------------------------------------------
-- General Name
----------------------------------------------------------------
data GenName = GenName
        { g_sort :: NameSort
        , g_name :: Name
        , g_loc :: Span
        }

instance Eq GenName where
        n1 == n2 = g_sort n1 == g_sort n2 && g_name n1 == g_name n2

instance Ord GenName where
        compare n1 n2 = compare (g_name n1) (g_name n2)

instance Show GenName where
        show n = show (g_name n)

data NameSort
        = External
        | Internal
        | System
        deriving (Eq, Show)

newName :: Name -> GenName
newName n = GenName System n NoSpan