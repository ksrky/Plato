{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Plato.Common.Name.Global where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc

import qualified Data.Map as M
import qualified Data.Text as T
import Prettyprinter

----------------------------------------------------------------
-- Global Name
----------------------------------------------------------------
data GlbName = GlbName {g_sort :: NameSort, g_name :: Name, g_loc :: Span}

instance Eq GlbName where
        n1 == n2 = g_sort n1 == g_sort n2 && g_name n1 == g_name n2

instance Ord GlbName where
        compare n1 n2 = compare (g_name n1) (g_name n2)

instance Show GlbName where
        show n = show (g_name n)

instance Pretty GlbName where
        pretty n = pretty (g_name n)

----------------------------------------------------------------
-- NameSort
----------------------------------------------------------------
data NameSort
        = External ModuleName
        | Internal
        | System
        deriving (Eq, Show)

externalName :: ModuleName -> Located Name -> GlbName
externalName modn (L sp n) = GlbName{g_sort = External modn, g_name = n, g_loc = sp}

internalName :: Located Name -> GlbName
internalName (L sp n) = GlbName{g_sort = Internal, g_name = n, g_loc = sp}

systemName :: Name -> GlbName
systemName n = GlbName{g_sort = System, g_name = n, g_loc = NoSpan}

newGlbName :: (T.Text -> Name) -> T.Text -> GlbName
newGlbName f t = GlbName{g_sort = System, g_name = f t, g_loc = NoSpan}

int2ext :: ModuleName -> GlbName -> GlbName
int2ext _ glbn@(GlbName External{} _ _) = glbn
int2ext modn glbn@(GlbName Internal _ _) = glbn{g_sort = External modn}
int2ext _ (GlbName System _ _) = unreachable "Local name shouldn't be appeared in top level"

----------------------------------------------------------------
-- Global Name Environment
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

insertGlbNameEnv :: ModuleName -> Located Name -> GlbNameEnv -> GlbNameEnv
insertGlbNameEnv modn n = M.insert (unLoc n) (externalName modn n)

lookupGlbNameEnv :: GlbNameEnv -> Located Name -> GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> glbn
        Nothing -> internalName (L sp n)
