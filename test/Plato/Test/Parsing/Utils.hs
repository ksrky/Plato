{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Parsing.Utils where

import Plato.Syntax.Parsing
import Plato.Types.Location
import Plato.Types.Name
import Plato.Types.Name.Global

import qualified Data.Text as T

-- See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html to know aboutn pattern synonims
pattern GVN :: T.Text -> Located GlbName
pattern GVN x <- L _ (GlbName Local (Name VarName x) _)

pattern VE :: T.Text -> LExpr GlbName
pattern VE x <- L _ (VarE (GVN x))

pattern Plus :: Located GlbName
pattern Plus <- GVN "+"

pattern Times :: Located GlbName
pattern Times <- GVN "*"

pattern Append :: Located GlbName
pattern Append <- GVN "++"

pattern GT :: Located GlbName
pattern GT <- GVN ">"