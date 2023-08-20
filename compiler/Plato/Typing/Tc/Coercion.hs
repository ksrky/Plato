{-# LANGUAGE DataKinds #-}

module Plato.Typing.Tc.Coercion where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data Coercion = Id | Coer (Expr 'Typed -> Expr 'Typed)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

infixr 9 .>, <.>

(.>) :: Coercion -> Expr 'Typed -> Expr 'Typed
Id .> e = e
Coer f .> e = f e

(<.>) :: Coercion -> Coercion -> Coercion
Id <.> f = f
f <.> Id = f
Coer f1 <.> Coer f2 = Coer (f1 . f2)

genTrans :: [Quant] -> Coercion
genTrans [] = Id
genTrans tvs = Coer (TAbsE tvs)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer (`TAppE` tys)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \e -> TAbsE sks1 (coercion .> TAppE e (map (VarT . fst) sks1))

prfunTrans :: (MonadReader e m, HasUniq e, MonadIO m) => [Quant] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coercion = return coercion
prfunTrans sks _ Id = return $ Coer $ \e -> TAbsE sks (TAppE e (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion = do
        id <- newVarIdent
        return $
                Coer $ \e ->
                        AbsEok
                                id
                                arg_ty
                                (coercion .> TAbsE sks (AppE (noLoc $ TAppE e (map (VarT . fst) sks)) (noLoc $ VarE id)))

deepskolTrans :: [Quant] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 <.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 <.> Coer (TAbsE skol_tvs) <.> coer2

funTrans :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- newVarIdent
        return $ Coer $ \f -> AbsEok id a2 (co_res .> AppE (noLoc f) (noLoc $ co_arg .> VarE id))