{-# LANGUAGE DataKinds #-}

module Plato.Typing.Tc.Coercion where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Typing

data Coercion = Id | Coer (Expr 'TcDone -> Expr 'TcDone)

instance Eq Coercion where
        Id == Id = True
        _ == _ = False

infixr 9 @@, >.>

(@@) :: Coercion -> Expr 'TcDone -> Expr 'TcDone
Id @@ e = e
Coer f @@ e = f e

(>.>) :: Coercion -> Coercion -> Coercion
Id >.> f = f
f >.> Id = f
Coer f1 >.> Coer f2 = Coer (f1 . f2)

genTrans :: [Quant] -> Coercion
genTrans [] = Id
genTrans tvs = Coer (TAbsE tvs . noLoc)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer ((`TAppE` tys) . noLoc)

prpolyTrans :: [Quant] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \e -> TAbsE sks1 (noLoc $ coercion @@ TAppE (noLoc e) (map (VarT . fst) sks1))

prfunTrans :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => [Quant] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coercion = return coercion
prfunTrans sks _ Id = return $ Coer $ \e -> TAbsE sks (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion = do
        id <- freshIdent $ str2varName "$x"
        return $
                Coer $ \e ->
                        AbsEok
                                id
                                arg_ty
                                (noLoc $ coercion @@ TAbsE sks (noLoc $ AppE (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks)) (noLoc $ VarE id)))

deepskolTrans :: [Quant] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 >.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 >.> Coer (TAbsE skol_tvs . noLoc) >.> coer2

funTrans :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- freshIdent $ str2varName "$x"
        return $ Coer $ \f -> AbsEok id a2 (noLoc $ co_res @@ AppE (noLoc f) (noLoc $ co_arg @@ VarE id))