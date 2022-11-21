module Plato.Types.Name.Global where

import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Name

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
        deriving (Show)

instance Eq GlbName where
        n1 == n2 = g_sort n1 == g_sort n2 && g_name n1 == g_name n2

instance Ord GlbName where
        compare n1 n2 = compare (g_name n1) (g_name n2)

-- instance Show GlbName where
--        show n = show (g_name n)

instance Pretty GlbName where
        pretty n = pretty (g_name n)

----------------------------------------------------------------
-- NameSort
----------------------------------------------------------------
data NameSort
        = External ModuleName
        | Internal DefLevel
        | Local
        deriving (Eq, Show)

data DefLevel
        = DefTop (Maybe ModuleName)
        | DefLevel Level
        deriving (Show)

instance Eq DefLevel where
        DefTop _ == DefTop _ = True
        DefLevel l1 == DefLevel l2 = l1 == l2
        _ == _ = False

type Level = Int

externalName :: ModuleName -> Located Name -> GlbName
externalName modn (L sp n) = GlbName{g_sort = External modn, g_name = n, g_loc = sp}

internalName :: DefLevel -> Located Name -> GlbName
internalName ds (L sp n) = GlbName{g_sort = Internal ds, g_name = n, g_loc = sp}

localName :: Located Name -> GlbName
localName (L sp n) = GlbName{g_sort = Local, g_name = n, g_loc = sp}

newGlbName :: NameSort -> Name -> GlbName
newGlbName ns n = GlbName{g_sort = ns, g_name = n, g_loc = NoSpan}

dummyGlbName :: GlbName
dummyGlbName = GlbName{g_sort = Local, g_name = Name{nameText = "", nameSpace = VarName}, g_loc = NoSpan}

updateGlbName :: GlbName -> GlbName
updateGlbName glbn = case g_sort glbn of
        Internal (DefTop (Just modn)) -> glbn{g_sort = External modn}
        Internal _ -> unreachable $ "Non-toplevel internal name occured: " ++ show glbn
        Local -> unreachable $ "Local name in top level: " ++ show glbn
        _ -> glbn

----------------------------------------------------------------
-- GlbNameEnv
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

extendGlbNameEnv :: DefLevel -> Located Name -> GlbNameEnv -> GlbNameEnv
extendGlbNameEnv ds n = M.insert (unLoc n) (internalName ds n)

extendGlbNameEnvList :: DefLevel -> [Located Name] -> GlbNameEnv -> GlbNameEnv
extendGlbNameEnvList ds = flip $ foldl $ flip (extendGlbNameEnv ds)

extendEnvLocal :: Located Name -> GlbNameEnv -> GlbNameEnv
extendEnvLocal n = M.insert (unLoc n) (localName n)

extendEnvListLocal :: [Located Name] -> GlbNameEnv -> GlbNameEnv
extendEnvListLocal = flip $ foldl $ flip extendEnvLocal

lookupGlbNameEnv :: GlbNameEnv -> Located Name -> GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> glbn{g_loc = sp}
        Nothing -> unreachable ""

filterGlbNameEnv :: [ModuleName] -> GlbNameEnv -> GlbNameEnv
filterGlbNameEnv imp_modns =
        M.filter
                ( \glbn -> case g_sort glbn of
                        External modn -> modn `elem` imp_modns
                        _ -> unreachable ""
                )

updateGlbNameEnv :: GlbNameEnv -> GlbNameEnv
updateGlbNameEnv = M.map updateGlbName