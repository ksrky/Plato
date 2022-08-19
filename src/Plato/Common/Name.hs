module Plato.Common.Name where

import Data.Char (isLower, isUpper)
import Data.List (intercalate)
import Data.Text (Text, pack, snoc, unpack)
import Plato.Common.Pretty (Pretty (..))

----------------------------------------------------------------
-- Name
----------------------------------------------------------------
data Name = Name {nameSpace :: NameSpace, nameText :: Text}

instance Eq Name where
        n1 == n2 = nameSpace n1 == nameSpace n2 && nameText n1 == nameText n2

instance Show Name where
        show (Name _ t) = unpack t

instance Pretty Name where
        pretty (Name _ t) = unpack t

data NameSpace
        = VarName
        | ConName
        | TyVarName
        | TyConName
        deriving (Eq, Show)

str2varName :: String -> Name
str2varName s = Name VarName (pack s)

str2conName :: String -> Name
str2conName s = Name ConName (pack s)

str2tyVarName :: String -> Name
str2tyVarName s = Name TyVarName (pack s)

str2tyConName :: String -> Name
str2tyConName s = Name TyConName (pack s)

appendstr :: Name -> String -> Name
appendstr n [] = n
appendstr (Name ns tx) [c] = Name ns (snoc tx c)
appendstr (Name ns tx) (c : s) = appendstr (Name ns (snoc tx c)) s

dummyVarName :: Name
dummyVarName = str2varName "_"

isVar :: Name -> Bool
isVar n = isLower $ head $ show n

isCon :: Name -> Bool
isCon n = isUpper $ head $ show n

nullName :: Name -> Bool
nullName n = null $ show n

entry :: Name
entry = str2varName "main"

----------------------------------------------------------------
-- Module
----------------------------------------------------------------
newtype ModuleName = ModuleName [Name] deriving (Eq, Show)

instance Pretty ModuleName where
        pretty (ModuleName modn) = intercalate "." (map pretty modn)

toPath :: ModuleName -> String
toPath (ModuleName modn) = intercalate "/" (map show modn) ++ ".plt"
