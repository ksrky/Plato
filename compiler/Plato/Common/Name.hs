module Plato.Common.Name where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
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
        pretty = viaShow

-- | NameSpace
data NameSpace
        = VarName
        | ConName
        | TyvarName
        | TyconName
        | GenName
        deriving (Eq, Ord, Show)

varName :: T.Text -> Name
varName = Name VarName

conName :: T.Text -> Name
conName = Name ConName

tyvarName :: T.Text -> Name
tyvarName = Name TyvarName

tyconName :: T.Text -> Name
tyconName = Name TyconName

genName :: T.Text -> Name
genName = Name GenName

str2varName :: String -> Name
str2varName = varName . T.pack

str2conName :: String -> Name
str2conName = conName . T.pack

str2tyvarName :: String -> Name
str2tyvarName = tyvarName . T.pack

str2tyconName :: String -> Name
str2tyconName = tyconName . T.pack

str2genName :: String -> Name
str2genName = genName . T.pack

dummyVN :: Name
dummyVN = str2varName "?"

wildcard :: Name
wildcard = str2varName "_"

----------------------------------------------------------------
-- Name Env
----------------------------------------------------------------
type NameMap a = M.Map Name a