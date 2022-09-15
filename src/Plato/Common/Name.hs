module Plato.Common.Name where

import Data.List (intercalate)
import qualified Data.Text as T

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
str2conName = varName . T.pack

str2tyvarName :: String -> Name
str2tyvarName = varName . T.pack

str2tyconName :: String -> Name
str2tyconName = varName . T.pack

entryPoint :: Name
entryPoint = varName $ T.pack "main"

----------------------------------------------------------------
-- Module Name
----------------------------------------------------------------
newtype ModuleName = ModuleName [Name] deriving (Eq)

instance Show ModuleName where
        show (ModuleName modn) = intercalate "." (map show modn)
