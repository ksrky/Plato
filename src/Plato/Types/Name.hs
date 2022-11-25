module Plato.Types.Name where

import Data.List (intercalate)
import qualified Data.Text as T
import Prettyprinter

----------------------------------------------------------------
-- Name
----------------------------------------------------------------
data Name = Name {nameSpace :: NameSpace, nameText :: T.Text}

instance Eq Name where
        n1 == n2 = nameSpace n1 == nameSpace n2 && nameText n1 == nameText n2

instance Ord Name where
        compare n1 n2 = compare (nameText n1) (nameText n2) <> compare (nameSpace n1) (nameSpace n2)

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
        | ModName
        deriving (Eq, Ord, Show)

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
-- Module Name
----------------------------------------------------------------
newtype ModuleName = ModuleName [T.Text] deriving (Eq)

instance Ord ModuleName where
        compare (ModuleName ts1) (ModuleName ts2) = compare ts1 ts2

instance Show ModuleName where
        show (ModuleName modn) = intercalate "." (map T.unpack modn)

instance Pretty ModuleName where
        pretty (ModuleName modn) = concatWith (surround dot) (map pretty modn)

mainModname :: ModuleName
mainModname = ModuleName [T.pack "Main"]

modn2name :: ModuleName -> Name
modn2name (ModuleName modn) = Name ModName (T.intercalate (T.pack ".") modn)

mod2path :: ModuleName -> FilePath
mod2path (ModuleName modn) = intercalate "/" (map T.unpack modn) ++ ".plt"