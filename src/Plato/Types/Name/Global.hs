module Plato.Types.Name.Global where

import Plato.Types.Location
import Plato.Types.Name

import qualified Data.Map as M
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
        | Local
        deriving (Eq, Show)

externalName :: ModuleName -> Located Name -> GlbName
externalName modn (L sp n) = GlbName{g_sort = External modn, g_name = n, g_loc = sp}

internalName :: Located Name -> GlbName
internalName (L sp n) = GlbName{g_sort = Internal, g_name = n, g_loc = sp}

localName :: Located Name -> GlbName
localName (L sp n) = GlbName{g_sort = Local, g_name = n, g_loc = sp}

newGlbName :: NameSort -> Name -> GlbName
newGlbName ns n = GlbName{g_sort = ns, g_name = n, g_loc = NoSpan}

----------------------------------------------------------------
-- GlbNameEnv
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

insertGlbNameEnv :: Located Name -> GlbNameEnv -> GlbNameEnv
insertGlbNameEnv n = M.insert (unLoc n) (internalName n)

lookupGlbNameEnv :: GlbNameEnv -> Located Name -> GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> glbn
        Nothing -> localName (L sp n)