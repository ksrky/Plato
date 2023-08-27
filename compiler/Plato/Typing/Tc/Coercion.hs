{-# LANGUAGE DataKinds #-}

module Plato.Typing.Tc.Coercion (
        Coercion,
        ap,
        genTrans,
        instTrans,
        prpolyTrans,
        prfunTrans,
        deepskolTrans,
        funTrans,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing
import Plato.Syntax.Typing.Helper

data Coercion = Id | Fn (Expr 'Typed -> Expr 'Typed)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

instance Semigroup Coercion where
        Id <> coer = coer
        coer <> Id = coer
        Fn f1 <> Fn f2 = Fn (f1 . f2)

instance Monoid Coercion where
        mempty = Id

ap :: Coercion -> Expr 'Typed -> Expr 'Typed
Id `ap` e = e
Fn f `ap` e = f e

genTrans :: [Quant] -> Coercion
genTrans [] = Id
genTrans tvs = Fn (TAbsE tvs)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Fn (`TAppE` tys)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coer = coer
prpolyTrans sks1 coer = Fn $ \e -> TAbsE sks1 (coer `ap` TAppE e (map (VarT . fst) sks1))

prfunTrans :: (MonadReader e m, HasUniq e, MonadIO m) => [Quant] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coer = return coer
prfunTrans sks _ Id = return $ Fn $ \e -> TAbsE sks (TAppE e (map (VarT . fst) sks))
prfunTrans sks arg_ty coer = do
        id <- newVarIdent
        let coer' e = coer `ap` TAbsE sks (AppE (noLoc $ TAppE e (map (VarT . fst) sks)) (noLoc $ VarE id))
        return $ Fn $ AbsEok id arg_ty . coer'

deepskolTrans :: [Quant] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 <> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 <> Fn (TAbsE skol_tvs) <> coer2

funTrans :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- newVarIdent
        return $ Fn $ \f -> AbsEok id a2 (co_res `ap` AppE (noLoc f) (noLoc $ co_arg `ap` VarE id))