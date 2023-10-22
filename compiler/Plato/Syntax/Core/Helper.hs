module Plato.Syntax.Core.Helper where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Plato.Common.Ident
import Plato.Common.Name
import Plato.Common.Uniq
import Plato.Syntax.Core

unit :: Term
unit = Label (genName "unit")

tUnit :: Type
tUnit = Enum [genName "unit"]

mkLam :: [Bind Type] -> Term -> Term
mkLam [] t = t
mkLam ((x, ty) : rest) t = Lam (x, ty) (mkLam rest t)

mkArr :: (MonadReader e m, HasUniq e, MonadIO m) => Type -> Type -> m Type
mkArr arg res = do
        idWC <- freshIdent wcName
        return $ Q Pi idWC arg res

mkQ :: PiSigma -> [Bind Type] -> Type -> Type
mkQ pisigma binds body = foldr (uncurry $ Q pisigma) body binds

mkPis :: [Bind Type] -> Type -> Type
mkPis = mkQ Pi

mkSigmas :: [Bind Type] -> Type -> Type
mkSigmas = mkQ Sigma

mkProduct :: [Term] -> Term
mkProduct [] = unit
mkProduct [t] = t
mkProduct ts = foldr1 Pair ts

mkTTuple :: (MonadReader e m, HasUniq e, MonadIO m) => [Type] -> m Type
mkTTuple [] = return tUnit
mkTTuple [ty] = return ty
mkTTuple (ty : tys) = do
        idWC <- freshIdent wcName
        Q Sigma idWC ty <$> mkTTuple tys

mkSplits ::
        forall e m.
        (MonadReader e m, HasUniq e, MonadIO m) =>
        Term ->
        [(Ident, Type)] ->
        Term ->
        m Term
mkSplits t vars body = loop t vars
    where
        loop :: Term -> [(Ident, Type)] -> m Term
        loop _ [] = return body
        loop t [(x, ty)] = return $ Let [Decl x ty, Defn x t] body
        loop t ((x, _) : [(y, _)]) = return $ Split t (x, y) body
        loop t ((x, _) : xs) = do
                idYZ <- freshIdent $ genName "ys"
                u <- loop (Var idYZ) xs
                return $ Split t (x, idYZ) u

decls :: Prog -> [Ident]
decls [] = []
decls (Decl x _ : p) = x : decls p
decls (Defn _ _ : p) = decls p