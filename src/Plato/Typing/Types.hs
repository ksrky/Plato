module Plato.Typing.Types where

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
        go (MetaType tv) acc
                | tv `elem` acc = acc
                | otherwise = tv : acc
        go (VarType _) acc = acc
        go (ConType _) acc = acc
        go (ArrType arg res) acc = go (unLoc arg) (go (unLoc res) acc)
        go (AllType _ ty) acc = go (unLoc ty) acc
        go (AppType fun arg) acc = go (unLoc fun) (go (unLoc arg) acc)
        go _ _ = unreachable "AbsType, RecType, RecordType, SumType"

freeTyVars :: [Type] -> [TyVar]
freeTyVars = foldr (go []) []
    where
        go :: [TyVar] -> Type -> [TyVar] -> [TyVar]
        go bound (VarType tv) acc
                | unLoc tv `elem` bound = acc
                | unLoc tv `elem` acc = acc
                | otherwise = unLoc tv : acc
        go _ (ConType _) acc = acc
        go _ (MetaType _) acc = acc
        go bound (ArrType arg res) acc = go bound (unLoc arg) (go bound (unLoc res) acc)
        go bound (AllType tvs ty) acc = go (map unLoc tvs ++ bound) (unLoc ty) acc
        go bound (AppType fun arg) acc = go bound (unLoc fun) (go bound (unLoc arg) acc)
        go _ _ _ = unreachable "AbsType, RecType, RecordType, SumType"

tyVarBndrs :: Rho -> [TyVar]
tyVarBndrs ty = nub (bndrs ty)
    where
        bndrs :: Type -> [TyVar]
        bndrs (AllType tvs body) = map unLoc tvs ++ bndrs (unLoc body)
        bndrs (ArrType arg res) = bndrs (unLoc arg) ++ bndrs (unLoc res)
        bndrs _ = []

tyVarName :: TyVar -> Name
tyVarName (BoundTv x) = x
tyVarName (SkolemTv x _) = x

varType :: Located Name -> Type
varType (L sp x) = VarType $ L sp (BoundTv x)

----------------------------------------------------------------
-- Substitution
----------------------------------------------------------------
type Env = [(TyVar, Tau)]

substTy :: [TyVar] -> [Type] -> Type -> Type
substTy tvs tys = subst_ty (tvs `zip` tys)

subst_ty :: Env -> Type -> Type
subst_ty env (ArrType arg res) = ArrType (subst_ty env <$> arg) (subst_ty env <$> res)
subst_ty env (VarType x) = fromMaybe (VarType x) (lookup (unLoc x) env)
subst_ty _ (ConType tc) = ConType tc
subst_ty _ (MetaType tv) = MetaType tv
subst_ty env (AllType ns rho) = AllType ns (subst_ty env' <$> rho)
    where
        env' = [(n, ty') | (n, ty') <- env, n `notElem` map unLoc ns]
subst_ty env (AppType fun arg) = ArrType (subst_ty env <$> fun) (subst_ty env <$> arg)
subst_ty _ _ = unreachable "AbsType, RecType, RecordType, SumType"