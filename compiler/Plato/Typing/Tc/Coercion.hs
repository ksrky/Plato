{-# LANGUAGE DataKinds #-}

module Plato.Typing.Tc.Coercion where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data Coercion = CoerId | CoerFn (Expr 'Typed -> Expr 'Typed)

instance Eq Coercion where
        CoerId == CoerId = True
        _ == _ = False

ap :: Coercion -> Expr 'Typed -> Expr 'Typed
CoerId `ap` e = e
CoerFn f `ap` e = f e

compose :: Coercion -> Coercion -> Coercion
CoerId `compose` f = f
f `compose` CoerId = f
CoerFn f1 `compose` CoerFn f2 = CoerFn (f1 . f2)

genTrans :: [Quant] -> Coercion
genTrans [] = CoerId
genTrans tvs = CoerFn (TAbsE tvs)

instTrans :: [Type] -> Coercion
instTrans [] = CoerId
instTrans tys = CoerFn (`TAppE` tys)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = CoerFn $ \e -> TAbsE sks1 (coercion `ap` TAppE e (map (VarT . fst) sks1))

prfunTrans :: (MonadReader e m, HasUniq e, MonadIO m) => [Quant] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coercion = return coercion
prfunTrans sks _ CoerId = return $ CoerFn $ \e -> TAbsE sks (TAppE e (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion = do
        id <- newVarIdent
        return $
                CoerFn $ \e ->
                        AbsEok
                                id
                                arg_ty
                                (coercion `ap` TAbsE sks (AppE (noLoc $ TAppE e (map (VarT . fst) sks)) (noLoc $ VarE id)))

deepskolTrans :: [Quant] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 `compose` coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 `compose` CoerFn (TAbsE skol_tvs) `compose` coer2

funTrans :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ CoerId CoerId = return CoerId
funTrans a2 co_arg co_res = do
        id <- newVarIdent
        return $ CoerFn $ \f -> AbsEok id a2 (co_res `ap` AppE (noLoc f) (noLoc $ co_arg `ap` VarE id))