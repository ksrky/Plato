module Plato.Common.Name.Global where

import Plato.Common.Name
import Plato.Common.SrcLoc

import qualified Data.Map as M
import qualified Data.Text as T
import Prettyprinter

----------------------------------------------------------------
-- Global Name
----------------------------------------------------------------
data GlbName = GlbName
        { g_sort :: NameSort
        , g_name :: Name
        , g_loc :: Span
        , g_occ :: Occurrence
        }

instance Eq GlbName where
        n1 == n2 = g_sort n1 == g_sort n2 && g_name n1 == g_name n2

instance Ord GlbName where
        compare n1 n2 = compare (g_name n1) (g_name n2)

instance Show GlbName where
        show n = show (g_name n)

instance Pretty GlbName where
        pretty n = pretty (g_name n)

----------------------------------------------------------------
-- Occurance
----------------------------------------------------------------
data Occurrence = OccLeft | OccRight deriving (Eq, Show)

----------------------------------------------------------------
-- NameSort
----------------------------------------------------------------
data NameSort
        = ExportedDef ModuleName
        | LocalDef
        | SystemDef
        deriving (Eq, Show)

exportedName :: ModuleName -> Occurrence -> Located Name -> GlbName
exportedName modn occ (L sp n) = GlbName{g_sort = ExportedDef modn, g_name = n, g_loc = sp, g_occ = occ}

localName :: Occurrence -> Located Name -> GlbName
localName occ (L sp n) = GlbName{g_sort = LocalDef, g_name = n, g_loc = sp, g_occ = occ}

systemName :: Occurrence -> Name -> GlbName
systemName occ n = GlbName{g_sort = SystemDef, g_name = n, g_loc = NoSpan, g_occ = occ}

newGlbName :: (T.Text -> Name) -> T.Text -> GlbName
newGlbName f t = GlbName{g_sort = SystemDef, g_name = f t, g_loc = NoSpan, g_occ = OccLeft}

----------------------------------------------------------------
-- Global Name Environment
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

insertGlbNameEnv :: ModuleName -> Located Name -> GlbNameEnv -> GlbNameEnv
insertGlbNameEnv modn n = M.insert (unLoc n) (exportedName modn OccLeft n)

lookupGlbNameEnv :: GlbNameEnv -> Located Name -> GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> glbn
        Nothing -> localName OccLeft (L sp n)
