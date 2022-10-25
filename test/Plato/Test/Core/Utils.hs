{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Core.Utils where

import Plato.Common.Name
import Plato.Common.SrcLoc

import qualified Data.Text as T

pattern NL :: a -> Located a
pattern NL x <- L _ x

pattern VN :: T.Text -> Name
pattern VN x <- Name VarName x

pattern CN :: T.Text -> Name
pattern CN x <- Name ConName x

pattern TVN :: T.Text -> Name
pattern TVN x <- Name TyvarName x

pattern TCN :: T.Text -> Name
pattern TCN x <- Name TyconName x