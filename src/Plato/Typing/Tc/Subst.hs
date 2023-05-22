module Plato.Typing.Tc.Subst (subst, apply) where

import Data.Map.Strict qualified as M

import Plato.Common.Error
import Plato.Syntax.Typing

type Subst = M.Map TyVar Tau

subst :: [TyVar] -> [Type] -> Type -> Type
subst tvs tys ty = let s = M.fromList (zip tvs tys) in apply s ty

apply :: Subst -> Type -> Type
apply s ty@(VarT tv) = M.findWithDefault ty tv s
apply _ ty@ConT{} = ty
apply s (ArrT arg res) = ArrT (apply s <$> arg) (apply s <$> res)
apply s (AllT tvs body) = AllT tvs $ apply (foldr (\(tv, _) -> M.delete tv) s tvs) <$> body
apply s (AppT fun arg) = AppT (apply s <$> fun) (apply s <$> arg)
apply _ AbsT{} = unreachable ""
-- apply s (AbsT var ann body) = AbsT var ann (apply s <$> body)
apply _ ty@MetaT{} = ty