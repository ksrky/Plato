module Plato.Common.Name where

import Data.Text

type Name = Text

str2name :: String -> Text
str2name = pack

name2str :: Text -> String
name2str = unpack

appendstr :: Text -> String -> Text
appendstr t [] = t
appendstr t [c] = snoc t c
appendstr t (c : s) = appendstr (snoc t c) s

dummyName :: Text
dummyName = str2name "?"

type ModuleName = [Name]
