module Plato.TypeCheck.Translate where

import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Typing

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

genTrans :: [(TyVar, Maybe Kind)] -> Coercion
genTrans [] = Id
genTrans tvs = Coer (TAbsE tvs . noLoc)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer ((`TAppE` tys) . noLoc)

prpolyTrans :: [(TyVar, Maybe Kind)] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \e -> TAbsE sks1 (noLoc $ coercion @@ TAppE (noLoc e) (map (VarT . fst) sks1))

prfunTrans :: [(TyVar, Maybe Kind)] -> Sigma -> Coercion -> Coercion
prfunTrans [] _ coercion = coercion
prfunTrans sks _ Id = Coer $ \e -> TAbsE sks (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion =
        let x = noLoc $ str2varName "$x"
            qx = noLoc $ TypName [] x
         in Coer $ \e ->
                AbsE
                        x
                        (Just arg_ty)
                        (noLoc $ coercion @@ TAbsE sks (noLoc $ AppE (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks)) (noLoc $ VarE qx)))

deepskolTrans :: [(TyVar, Maybe Kind)] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 >.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 >.> Coer (TAbsE skol_tvs . noLoc) >.> coer2

funTrans :: Sigma -> Coercion -> Coercion -> Coercion
funTrans _ Id Id = Id
funTrans a2 co_arg co_res =
        let x = noLoc $ str2varName "$x"
            qx = noLoc $ TypName [] x
         in Coer $ \f -> AbsE x (Just a2) (noLoc $ co_res @@ AppE (noLoc f) (noLoc $ co_arg @@ VarE qx))