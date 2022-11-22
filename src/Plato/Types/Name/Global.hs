module Plato.Types.Name.Global where

import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Name

import Control.Exception.Safe
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
        = TopLevel ModuleName
        | LocalTop Level
        | Local
        deriving (Eq, Show)

type Level = Int

toplevelName :: ModuleName -> Located Name -> GlbName
toplevelName modn (L sp n) = GlbName{g_sort = TopLevel modn, g_name = n, g_loc = sp}

localtopName :: Level -> Located Name -> GlbName
localtopName lev (L sp n) = GlbName{g_sort = LocalTop lev, g_name = n, g_loc = sp}

localName :: Located Name -> GlbName
localName (L sp n) = GlbName{g_sort = Local, g_name = n, g_loc = sp}

newGlbName :: NameSort -> Name -> GlbName
newGlbName ns n = GlbName{g_sort = ns, g_name = n, g_loc = NoSpan}

dummyGlbName :: GlbName
dummyGlbName = GlbName{g_sort = Local, g_name = Name{nameText = "", nameSpace = VarName}, g_loc = NoSpan}

----------------------------------------------------------------
-- GlbNameEnv
----------------------------------------------------------------
type GlbNameEnv = M.Map Name GlbName -- stores defined global name

extendEnvExt :: ModuleName -> Located Name -> GlbNameEnv -> GlbNameEnv
extendEnvExt modn n = M.insert (unLoc n) (toplevelName modn n)

extendEnvListExt :: ModuleName -> [Located Name] -> GlbNameEnv -> GlbNameEnv
extendEnvListExt modn = flip $ foldl $ flip (extendEnvExt modn)

extendEnvInt :: Level -> Located Name -> GlbNameEnv -> GlbNameEnv
extendEnvInt lev n = M.insert (unLoc n) (localtopName lev n)

extendEnvListInt :: Level -> [Located Name] -> GlbNameEnv -> GlbNameEnv
extendEnvListInt lev = flip $ foldl $ flip (extendEnvInt lev)

extendEnvLocal :: Located Name -> GlbNameEnv -> GlbNameEnv
extendEnvLocal n = M.insert (unLoc n) (localName n)

extendEnvListLocal :: [Located Name] -> GlbNameEnv -> GlbNameEnv
extendEnvListLocal = flip $ foldl $ flip extendEnvLocal

lookupGlbNameEnv :: MonadThrow m => GlbNameEnv -> Located Name -> m GlbName
lookupGlbNameEnv glbenv (L sp n) = case M.lookup n glbenv of
        Just glbn -> return glbn{g_loc = sp}
        Nothing -> throwLocErr sp $ hsep ["Not in scope:", pretty n]

filterGlbNameEnv :: [ModuleName] -> GlbNameEnv -> GlbNameEnv
filterGlbNameEnv imp_modns =
        M.filter
                ( \glbn -> case g_sort glbn of
                        TopLevel modn -> modn `elem` imp_modns
                        _ -> unreachable "filterGlbNameEnv"
                )
