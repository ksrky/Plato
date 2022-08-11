module Plato.Common.Name where

import Data.Char (isLower, isUpper)
import Data.Text (Text, pack, snoc, unpack)

----------------------------------------------------------------
-- Name
----------------------------------------------------------------
data Name = Name {nameSpace :: NameSpace, nameText :: Text} deriving (Eq)

instance Show Name where
        show (Name _ t) = unpack t

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
str2tyVarName s = Name TyConName (pack s)

str2tyConName :: String -> Name
str2tyConName s = Name TyConName (pack s)

name2str :: Name -> String
name2str = unpack . nameText

appendstr :: Name -> String -> Name
appendstr n [] = n
appendstr (Name ns tx) [c] = Name ns (snoc tx c)
appendstr (Name ns tx) (c : s) = appendstr (Name ns (snoc tx c)) s

dummyVarName :: Name
dummyVarName = str2varName "?v"

isVar :: Name -> Bool
isVar n = isLower $ head $ name2str n

isCon :: Name -> Bool
isCon n = isUpper $ head $ name2str n

nullName :: Name -> Bool
nullName n = null $ name2str n

----------------------------------------------------------------
-- Module
----------------------------------------------------------------
type ModuleName = [Name]
