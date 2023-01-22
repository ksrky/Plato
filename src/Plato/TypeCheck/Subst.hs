module Plato.TypeCheck.Subst where

import qualified Data.Map.Strict as M

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
apply s (AbsT var mbkn body) = AbsT var mbkn (apply s <$> body)
apply s (RecT var kn body) = RecT var kn (apply s <$> body)
apply s (RecordT fields) = RecordT (map (\(l, ty) -> (l, apply s <$> ty)) fields)
apply s (SumT fields) = SumT (map (\(l, tys) -> (l, map (apply s <$>) tys)) fields)
apply _ ty@MetaT{} = ty