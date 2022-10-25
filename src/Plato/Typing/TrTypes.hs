module Plato.Typing.TrTypes where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Syntax.Typing

import Data.IORef
import Data.List
import Data.Maybe
import Plato.Common.Error

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv Name
        | SkolemTv Name Uniq
        deriving (Show)

data MetaTv = Meta Uniq TyRef

type TyRef = IORef (Maybe Tau)

type Uniq = Int

instance Eq MetaTv where
        (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Show MetaTv where
        show (Meta u _) = "$" ++ show u

instance Eq TyVar where
        (BoundTv s1) == (BoundTv s2) = s1 == s2
        (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
        _ == _ = False

metaTvs :: [Type] -> [MetaTv]
metaTvs = foldr go []
    where
        go :: Type -> [MetaTv] -> [MetaTv]
        go (MetaT tv) acc
                | tv `elem` acc = acc
                | otherwise = tv : acc
        go (VarT _) acc = acc
        go (ConT _) acc = acc
        go (ArrT arg res) acc = go (unLoc arg) (go (unLoc res) acc)
        go (AllT _ ty) acc = go (unLoc ty) acc
        go (AppT fun arg) acc = go (unLoc fun) (go (unLoc arg) acc)
        go _ _ = unreachable "AbsType, RecType, RecordType, SumType"

freeTyVars :: [Type] -> [TyVar]
freeTyVars = foldr (go []) []
    where
        go :: [TyVar] -> Type -> [TyVar] -> [TyVar]
        go bound (VarT tv) acc
                | unLoc tv `elem` bound = acc
                | unLoc tv `elem` acc = acc
                | otherwise = unLoc tv : acc
        go _ (ConT _) acc = acc
        go _ (MetaT _) acc = acc
        go bound (ArrT arg res) acc = go bound (unLoc arg) (go bound (unLoc res) acc)
        go bound (AllT tvs ty) acc = go (map (unLoc . fst) tvs ++ bound) (unLoc ty) acc
        go bound (AppT fun arg) acc = go bound (unLoc fun) (go bound (unLoc arg) acc)
        go _ _ _ = unreachable "AbsType, RecType, RecordType, SumType"

tyVarBndrs :: Rho -> [TyVar]
tyVarBndrs ty = nub (bndrs ty)
    where
        bndrs :: Type -> [TyVar]
        bndrs (AllT tvs body) = map (unLoc . fst) tvs ++ bndrs (unLoc body)
        bndrs (ArrT arg res) = bndrs (unLoc arg) ++ bndrs (unLoc res)
        bndrs _ = []

tyVarName :: TyVar -> Name
tyVarName (BoundTv x) = x
tyVarName (SkolemTv x _) = x

varType :: Located Name -> Type
varType (L sp x) = VarT $ L sp (BoundTv x)

----------------------------------------------------------------
-- Substitution
----------------------------------------------------------------
type Env = [(TyVar, Tau)]

substTy :: [TyVar] -> [Type] -> Type -> Type
substTy tvs tys = subst_ty (tvs `zip` tys)

subst_ty :: Env -> Type -> Type
subst_ty env (ArrT arg res) = ArrT (subst_ty env <$> arg) (subst_ty env <$> res)
subst_ty env (VarT x) = fromMaybe (VarT x) (lookup (unLoc x) env)
subst_ty _ (ConT tc) = ConT tc
subst_ty _ (MetaT tv) = MetaT tv
subst_ty env (AllT tvs rho) = AllT tvs (subst_ty env' <$> rho)
    where
        env' = [(n, ty') | (n, ty') <- env, n `notElem` map (unLoc . fst) tvs]
subst_ty env (AppT fun arg) = ArrT (subst_ty env <$> fun) (subst_ty env <$> arg)
subst_ty _ ty = ty