{-# LANGUAGE DataKinds #-}

module Plato.Typing.Tc.Coercion (
        Coercion,
        unCoer,
        genTrans,
        instTrans,
        prpolyTrans,
        prfunTrans,
        deepskolTrans,
        funTrans,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

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

unCoer :: Coercion -> Expr 'Typed -> Expr 'Typed
unCoer Id = id
unCoer (Fn f) = f

genTrans :: [Quant] -> Coercion
genTrans [] = Id
genTrans qnts = Fn (TAbsE qnts)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans arg_tys = Fn (`TAppE` arg_tys)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coer = coer
prpolyTrans qnts coer = Fn $ \e -> TAbsE qnts (unCoer coer $ TAppE e (map (VarT . fst) qnts))

prfunTrans :: (MonadReader e m, HasUniq e, MonadIO m) => [Quant] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coer = return coer
prfunTrans _ _ Id = return Id
prfunTrans qnts arg_ty coer = do
        id <- labelVarId "pf"
        let coer' e = unCoer coer $ TAbsE qnts (AppE (TAppE e (map (VarT . fst) qnts)) (VarE id))
        return $ Fn $ AbsE id arg_ty . coer'

deepskolTrans :: [Quant] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 <> coer2
deepskolTrans qns coer1 coer2 = coer1 <> Fn (TAbsE qns) <> coer2

funTrans :: (MonadReader e m, HasUniq e, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- labelVarId "fu"
        return $ Fn $ \f -> AbsE id a2 (unCoer co_res $ AppE f (unCoer co_arg $ VarE id))