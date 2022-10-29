{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Typing.Utils where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing
import Plato.Typing.TcTypes

import qualified Data.Text as T
import Plato.Common.GlbName

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

pattern VE :: T.Text -> Expr
pattern VE x <- VarE (VN x)

pattern CE :: T.Text -> Expr
pattern CE x <- VarE (CN x)

pattern TV :: T.Text -> TyVar
pattern TV x <- BoundTv (TVN x)

pattern STV :: T.Text -> TyVar
pattern STV x <- SkolemTv (TVN x) _

pattern VT :: T.Text -> Type
pattern VT x <- VarT (TV x)

pattern SVT :: T.Text -> Type
pattern SVT x <- VarT (STV x)

pattern CT :: T.Text -> Type
pattern CT x <- ConT (TCN x)

pattern CP :: T.Text -> [Pat] -> Pat
pattern CP x ps <- ConP (CN x) ps

pattern ABSE :: T.Text -> Expr -> Expr
pattern ABSE x e <- AbsE (VN x) Nothing e

pattern ALLT :: T.Text -> Type -> Type
pattern ALLT x ty <- AllT [(TV x, _)] ty