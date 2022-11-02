module Plato.Common.GlbName where

import Plato.Common.Name
import Plato.Common.SrcLoc

import Prettyprinter

----------------------------------------------------------------
-- Global Name
----------------------------------------------------------------
data GlbName = GlbName
        { g_sort :: NameSort
        , g_name :: Name
        , g_loc :: Span
        }

instance Eq GlbName where
        n1 == n2 = g_sort n1 == g_sort n2 && g_name n1 == g_name n2

instance Ord GlbName where
        compare n1 n2 = compare (g_name n1) (g_name n2)

instance Show GlbName where
        show n = show (g_name n)

instance Pretty GlbName where
        pretty n = pretty (g_name n)

data NameSort
        = External
        | Internal
        | System
        deriving (Eq, Show)

newName :: Name -> GlbName
newName n = GlbName System n NoSpan

toSystem :: GlbName -> GlbName
toSystem n = n{g_sort = System}