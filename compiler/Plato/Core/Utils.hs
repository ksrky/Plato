module Plato.Core.Utils where

import Plato.Common.Name
import Plato.Syntax.Core

unit :: Term
unit = Label (genName "unit")

tUnit :: Type
tUnit = Enum [genName "unit"]

mkLam :: [Bind Type] -> Term -> Term
mkLam [] t = t
mkLam ((x, ty) : rest) t = Lam (x, ty) (mkLam rest t)

mkArr :: Type -> Type -> Type
mkArr ty = Q Pi (wcName, ty)

mkQ :: PiSigma -> [Bind Type] -> Type -> Type
mkQ pisigma binds body = foldr (Q pisigma) body binds

mkPis :: [Bind Type] -> Type -> Type
mkPis = mkQ Pi

mkSigmas :: [Bind Type] -> Type -> Type
mkSigmas = mkQ Sigma

mkProduct :: [Term] -> Term
mkProduct [] = unit
mkProduct [t] = t
mkProduct ts = foldr1 Pair ts

mkTTuple :: [Type] -> Type
mkTTuple [] = tUnit
mkTTuple tys = foldr1 (\ty -> Q Sigma (wcName, ty)) tys

mkSplits :: Term -> [Name] -> Term -> Term
mkSplits t vars body = loop t (reverse vars)
    where
        loop :: Term -> [Name] -> Term
        loop _ [] = body
        loop t [x] = Let [Defn x t] body
        loop t (x : [y]) = Split t (x, (y, body))
        loop t (x : xs) = let yz = genName "yz" in Split t (x, (yz, loop (Var yz) xs))

mkUnfold :: Term -> Term
mkUnfold t = let x = genName "x" in Unfold (x, t) (Var x)