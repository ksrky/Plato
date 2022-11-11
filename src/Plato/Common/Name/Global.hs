module Plato.Common.Name.Global where

import Plato.Common.Name
import Plato.Common.SrcLoc

import qualified Data.Map as M
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

----------------------------------------------------------------
-- NameSort
----------------------------------------------------------------
data NameSort
        = ExportedDef ModuleName
        | LocalDef
        | SystemDef
        deriving (Eq, Show)

exportedName :: ModuleName -> Located Name -> GlbName
exportedName modn (L sp n) = GlbName{g_sort = ExportedDef modn, g_name = n, g_loc = sp}

localName :: Located Name -> GlbName
localName (L sp n) = GlbName{g_sort = LocalDef, g_name = n, g_loc = sp}

----------------------------------------------------------------
-- Global Name Environment
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

insertGlbNameEnv :: Maybe ModuleName -> Located Name -> GlbNameEnv -> GlbNameEnv
insertGlbNameEnv mb_modn n = case mb_modn of
        Just modn -> M.insert (unLoc n) (exportedName modn n)
        Nothing -> M.insert (unLoc n) (localName n)

lookupGlbNameEnv :: GlbNameEnv -> Located Name -> GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> glbn
        Nothing -> localName (L sp n)

