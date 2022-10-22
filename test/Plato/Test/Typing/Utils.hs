{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Typing.Utils where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TrTypes

import qualified Data.Text as T

pattern NL :: a -> Located a
pattern NL x <- L _ x

pattern VN :: T.Text -> Located Name
pattern VN x <- NL (Name VarName x)

pattern CN :: T.Text -> Located Name
pattern CN x <- NL (Name ConName x)

pattern TVN :: T.Text -> Located Name
pattern TVN x <- NL (Name TyvarName x)

pattern TCN :: T.Text -> Located Name
pattern TCN x <- NL (Name TyconName x)

pattern VE :: T.Text -> Located Expr
pattern VE x <- NL (VarE (VN x))

pattern CE :: T.Text -> Located Expr
pattern CE x <- NL (VarE (CN x))

pattern TV :: T.Text -> Located TyVar
pattern TV x <- NL (BoundTv (Name TyvarName x))

pattern VT :: T.Text -> Located Type
pattern VT x <- NL (VarT (TV x))

pattern CT :: T.Text -> Located Type
pattern CT x <- NL (ConT (TCN x))

pattern CP :: T.Text -> [Located Pat] -> Located Pat
pattern CP x ps <- NL (ConP (CN x) ps)
