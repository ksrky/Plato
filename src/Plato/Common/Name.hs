module Plato.Common.Name where

import Data.Text

type Name = Text

strtoname :: String -> Text
strtoname = pack

nametostr :: Text -> String
nametostr = unpack
