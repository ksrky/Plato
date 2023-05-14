module Plato.Common.Name where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Prettyprinter

----------------------------------------------------------------
-- Name
----------------------------------------------------------------
data Name = Name
        { nameSpace :: NameSpace
        , nameText :: T.Text
        }

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

dummyVN :: Name
dummyVN = str2varName "?"

wildcard :: Name
wildcard = str2varName "_"

----------------------------------------------------------------
-- Name Env
----------------------------------------------------------------
type NameMap a = M.Map Name a