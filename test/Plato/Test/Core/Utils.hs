{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Core.Utils where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc

import qualified Data.Text as T

pattern NL :: a -> Located a
pattern NL x <- L _ x

pattern VN :: T.Text -> GlbName
pattern VN x <- GlbName _ (Name VarName x) _

pattern CN :: T.Text -> GlbName
pattern CN x <- GlbName _ (Name ConName x) _

pattern TVN :: T.Text -> GlbName
pattern TVN x <- GlbName _ (Name TyvarName x) _

pattern TCN :: T.Text -> GlbName
pattern TCN x <- GlbName _ (Name TyconName x) _