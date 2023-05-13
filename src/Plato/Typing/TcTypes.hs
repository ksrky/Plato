module Plato.Typing.TcTypes where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Typing

metaTvs :: Type -> S.Set MetaTv
metaTvs VarT{} = S.empty
metaTvs ConT{} = S.empty
metaTvs (ArrT arg res) = metaTvs (unLoc arg) `S.union` metaTvs (unLoc res)
metaTvs (AllT _ body) = metaTvs (unLoc body)
metaTvs (AppT fun arg) = metaTvs (unLoc fun) `S.union` metaTvs (unLoc arg)
metaTvs (MetaT tv) = S.singleton tv
metaTvs _ = unreachable "TypeCheck.Utils.metaTvs"

freeTvs :: Type -> S.Set TyVar
freeTvs (VarT tv) = S.singleton tv
freeTvs ConT{} = S.empty
freeTvs (ArrT arg res) = freeTvs (unLoc arg) `S.union` freeTvs (unLoc res)
freeTvs (AllT tvs ty) = S.fromList (map fst tvs) `S.union` freeTvs (unLoc ty)
freeTvs (AppT fun arg) = freeTvs (unLoc fun) `S.union` freeTvs (unLoc arg)
freeTvs MetaT{} = S.empty
freeTvs _ = unreachable "TypeCheck.Utils.freeTvs"

tyVarBndrs :: Rho -> [TyVar]
tyVarBndrs ty = nub (bndrs ty)
    where
        bndrs :: Type -> [TyVar]
        bndrs (AllT tvs body) = map fst tvs ++ bndrs (unLoc body)
        bndrs (ArrT arg res) = bndrs (unLoc arg) ++ bndrs (unLoc res)
        bndrs _ = []

tyVarName :: TyVar -> Name
tyVarName = unLoc . tyVarLName

tyVarLName :: TyVar -> LName
tyVarLName (BoundTv x) = x
tyVarLName (SkolemTv x _) = x

----------------------------------------------------------------
-- Substitution
----------------------------------------------------------------
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
-- apply s (RecT var kn body) = RecT var kn (apply s <$> body)
-- apply s (RecordT fields) = RecordT (map (\(l, ty) -> (l, apply s <$> ty)) fields)
-- apply s (SumT fields) = SumT (map (\(l, tys) -> (l, map (apply s <$>) tys)) fields)
apply _ ty@MetaT{} = ty
apply _ ty = ty -- tmp