module Plato.TypeCheck.Translate where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Global
import Plato.Common.Location
import Plato.Common.Name
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Ident as Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Path as Path
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

genTrans :: [(TyVar, Maybe Kind)] -> Coercion
genTrans [] = Id
genTrans tvs = Coer (TAbsE tvs . noLoc)

instTrans :: [Type] -> Coercion
instTrans [] = Id
instTrans tys = Coer ((`TAppE` tys) . noLoc)

prpolyTrans :: [(TyVar, Maybe Kind)] -> Coercion -> Coercion
prpolyTrans [] coercion = coercion
prpolyTrans sks1 coercion = Coer $ \e -> TAbsE sks1 (noLoc $ coercion @@ TAppE (noLoc e) (map (VarT . fst) sks1))

prfunTrans :: (MonadReader glb m, HasUnique glb, MonadIO m) => [(TyVar, Maybe Kind)] -> Sigma -> Coercion -> m Coercion
prfunTrans [] _ coercion = return coercion
prfunTrans sks _ Id = return $ Coer $ \e -> TAbsE sks (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks))
prfunTrans sks arg_ty coercion = do
        id <- Ident.fresh $ str2varName "$x"
        return $
                Coer $ \e ->
                        AbsE
                                id
                                (Just arg_ty)
                                (noLoc $ coercion @@ TAbsE sks (noLoc $ AppE (noLoc $ TAppE (noLoc e) (map (VarT . fst) sks)) (noLoc $ VarE (Path.PIdent id))))

deepskolTrans :: [(TyVar, Maybe Kind)] -> Coercion -> Coercion -> Coercion
deepskolTrans [] coer1 coer2 = coer1 >.> coer2
deepskolTrans skol_tvs coer1 coer2 = coer1 >.> Coer (TAbsE skol_tvs . noLoc) >.> coer2

funTrans :: (MonadReader glb m, HasUnique glb, MonadIO m) => Sigma -> Coercion -> Coercion -> m Coercion
funTrans _ Id Id = return Id
funTrans a2 co_arg co_res = do
        id <- Ident.fresh $ str2varName "$x"
        return $ Coer $ \f -> AbsE id (Just a2) (noLoc $ co_res @@ AppE (noLoc f) (noLoc $ co_arg @@ VarE (Path.PIdent id)))