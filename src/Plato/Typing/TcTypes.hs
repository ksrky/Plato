module Plato.Typing.TcTypes where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Data.List (nub)
import Data.Maybe (fromMaybe)

metaTvs :: [Type] -> [MetaTv]
metaTvs = foldr go []
  where
    go :: Type -> [MetaTv] -> [MetaTv]
    go (MetaT tv) acc
        | tv `elem` acc = acc
        | otherwise = tv : acc
    go (VarT _) acc = acc
    go (ConT _) acc = acc
    go (ArrT arg res) acc = go arg (go res acc)
    go (AllT _ ty) acc = go ty acc
    go (AppT fun arg) acc = go fun (go arg acc)
    go _ _ = unreachable ""

freeTyVars :: [Type] -> [TyVar]
freeTyVars = foldr (go []) []
  where
    go :: [TyVar] -> Type -> [TyVar] -> [TyVar]
    go bound (VarT tv) acc
        | tv `elem` bound = acc
        | tv `elem` acc = acc
        | otherwise = tv : acc
    go _ (ConT _) acc = acc
    go _ (MetaT _) acc = acc
    go bound (ArrT arg res) acc = go bound arg (go bound res acc)
    go bound (AllT tvs ty) acc = go (map fst tvs ++ bound) ty acc
    go bound (AppT fun arg) acc = go bound fun (go bound arg acc)
    go _ _ _ = unreachable ""

tyVarBndrs :: Rho -> [TyVar]
tyVarBndrs ty = nub (bndrs ty)
  where
    bndrs :: Type -> [TyVar]
    bndrs (AllT tvs body) = map fst tvs ++ bndrs body
    bndrs (ArrT arg res) = bndrs arg ++ bndrs res
    bndrs _ = []

tyVarName :: TyVar -> Name
tyVarName = unLoc . tyVarLName

tyVarLName :: TyVar -> LName
tyVarLName (BoundTv x) = x
tyVarLName (SkolemTv x _) = x

tyVarGlbName :: TyVar -> GlbName
tyVarGlbName (BoundTv n) = internalName n
tyVarGlbName (SkolemTv n _) = internalName n

----------------------------------------------------------------
-- Substitution
----------------------------------------------------------------
type Env = [(TyVar, Tau)]

substTy :: [TyVar] -> [Type] -> Type -> Type
substTy tvs tys = subst_ty (tvs `zip` tys)

subst_ty :: Env -> Type -> Type
subst_ty env (VarT x) = fromMaybe (VarT x) (lookup x env)
subst_ty _ (ConT tc) = ConT tc
subst_ty env (ArrT arg res) = ArrT (subst_ty env arg) (subst_ty env res)
subst_ty env (AppT fun arg) = AppT (subst_ty env fun) (subst_ty env arg)
subst_ty _ (MetaT tv) = MetaT tv
subst_ty env (AllT tvs rho) = AllT tvs (subst_ty env' rho)
  where
    env' = [(n, ty') | (n, ty') <- env, n `notElem` map fst tvs]
subst_ty _ ty = ty