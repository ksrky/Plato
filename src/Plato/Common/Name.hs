module Plato.Common.Name where

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
newtype ModuleName = ModuleName T.Text deriving (Eq)

instance Ord ModuleName where
        compare (ModuleName t1) (ModuleName t2) = compare t1 t2

instance Show ModuleName where
        show (ModuleName modn) = T.unpack modn

instance Pretty ModuleName where
        pretty (ModuleName modn) = pretty modn

mainModname :: ModuleName
mainModname = ModuleName (T.pack "Main")

dummyModname :: ModuleName
dummyModname = ModuleName (T.pack "_")

modn2name :: ModuleName -> Name
modn2name (ModuleName modn) = Name ModName modn

mod2path :: ModuleName -> FilePath
mod2path (ModuleName modn) = show modn ++ ".plt"