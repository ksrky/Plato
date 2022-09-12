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

----------------------------------------------------------------
-- Module Name
----------------------------------------------------------------
newtype ModuleName = ModuleName [Name] deriving (Eq)

instance Show ModuleName where
        show (ModuleName modn) = intercalate "." (map show modn)
