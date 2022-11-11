module Plato.Common.Name where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Text as T
import Prettyprinter

----------------------------------------------------------------
-- Name
----------------------------------------------------------------
data Name = Name {nameSpace :: NameSpace, nameText :: T.Text}

instance Eq Name where
        n1 == n2 = nameSpace n1 == nameSpace n2 && nameText n1 == nameText n2

instance Ord Name where
        compare n1 n2 = compare (nameText n1) (nameText n2)

instance Show Name where
        show (Name _ t) = T.unpack t

instance Pretty Name where
        pretty n = viaShow n

-- | NameSpace
data NameSpace
        = VarName
        | ConName
        | TyvarName
        | TyconName
        deriving (Eq, Show)

varName :: T.Text -> Name
varName = Name VarName

conName :: T.Text -> Name
conName = Name ConName

tyvarName :: T.Text -> Name
tyvarName = Name TyvarName

tyconName :: T.Text -> Name
tyconName = Name TyconName

str2varName :: String -> Name
str2varName = varName . T.pack

str2conName :: String -> Name
str2conName = conName . T.pack

str2tyvarName :: String -> Name
str2tyvarName = tyvarName . T.pack

str2tyconName :: String -> Name
str2tyconName = tyconName . T.pack

----------------------------------------------------------------
-- NameEnv
----------------------------------------------------------------
type NameEnv = M.Map Name

lookup :: Name -> NameEnv a -> Maybe a
lookup = M.lookup

member :: Name -> NameEnv a -> Bool
member = M.member

----------------------------------------------------------------
-- Module Name
----------------------------------------------------------------
newtype ModuleName = ModuleName [T.Text] deriving (Eq)

instance Ord ModuleName where
        compare (ModuleName ts1) (ModuleName ts2) = compare ts1 ts2

instance Show ModuleName where
        show (ModuleName modn) = intercalate "." (map T.unpack modn)

instance Pretty ModuleName where
        pretty (ModuleName modn) = concatWith (surround dot) (map pretty modn)

mod2conName :: ModuleName -> Name
mod2conName (ModuleName modn) = Name ConName (':' `T.cons` T.intercalate (T.pack ".") modn)

mod2tyconName :: ModuleName -> Name
mod2tyconName (ModuleName modn) = Name TyconName (T.intercalate (T.pack ".") modn)

mod2path :: ModuleName -> FilePath
mod2path (ModuleName modn) = intercalate "/" (map T.unpack modn) ++ ".plt"
