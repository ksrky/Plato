module Plato.Common.Name where

import Plato.Common.Error

import Control.Exception.Safe
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

dummyVN :: Name
dummyVN = str2varName "?"

wildcard :: Name
wildcard = str2varName "_"

----------------------------------------------------------------
-- Module Name
----------------------------------------------------------------
newtype ModuleName = ModuleName T.Text deriving (Eq)

instance Ord ModuleName where
        compare (ModuleName ts1) (ModuleName ts2) = compare ts1 ts2

instance Show ModuleName where
        show (ModuleName modn) = T.unpack modn

instance Pretty ModuleName where
        pretty (ModuleName modn) = pretty modn

filePath2modName :: MonadThrow m => FilePath -> m ModuleName
filePath2modName fname = case reverse fname of
        't' : 'l' : 'p' : '.' : ndom -> return $ ModuleName (T.pack (reverse ndom))
        _ -> throwFatal "file name must ends `.plt`"

modn2name :: ModuleName -> Name
modn2name (ModuleName modn) = Name ModName modn

modn2names :: ModuleName -> [Name]
modn2names (ModuleName modn) = map (Name ModName) (T.splitOn "." modn)

mod2path :: ModuleName -> FilePath
mod2path (ModuleName modn) = T.unpack (T.replace "." "/" modn) ++ ".plt"