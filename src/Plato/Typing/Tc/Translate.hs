module Plato.Typing.Tc.Translate where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Type

data Coercion = Id | Coer (Expr -> Expr)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

infixr 9 @@, >.>

(@@) :: Coercion -> Expr -> Expr
Id @@ e = e
Coer f @@ e = f e

(>.>) :: Coercion -> Coercion -> Coercion
Id >.> f = f
f >.> Id = f
Coer f1 >.> Coer f2 = Coer (f1 . f2)

genTrans :: [Quant] -> Coercion
genTrans [] = Id
genTrans qnts = Coer (TAbsE (map (unTyVar . fst) qnts)) -- tmp

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer (`TAppE` tys)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \e -> TAbsE (map (unTyVar . fst) sks1) (coercion @@ TAppE e (map (VarT . fst) sks1))

{-}
prfunTrans :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => [(TyVar, Maybe Kind)] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coercion = return coercion
prfunTrans sks _ Id = return $ Coer $ \e -> TAbsE (map (unTyVar . fst) sks) (TAppE e (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion = do
        let id = noLoc $ str2varName "$x"
        return $
                Coer $ \e ->
                        AbsE
                                id
                                (Just arg_ty)
                                (coercion @@ TAbsE (map (unTyVar . fst) sks) (AppE (TAppE e (map (VarT . fst) sks)) (VarE id)))

deepskolTrans :: [(TyVar, Maybe Kind)] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 >.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 >.> Coer (TAbsE skol_tvs . noLoc) >.> coer2

funTrans :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- freshIdent $ str2varName "$x"
        return $ Coer $ \f -> AbsE id (Just a2) (noLoc $ co_res @@ AppE (noLoc f) (noLoc $ co_arg @@ VarE (PIdent id)))-}