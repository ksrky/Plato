module Plato.Core.Subst where

import Plato.Core.Debug
import Plato.Syntax.Core

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
tymap :: (Int -> Info -> Int -> Type) -> Int -> Type -> Type
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar fi x -> onvar c fi x
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyRec tyX knK1 tyT2 -> TyRec tyX knK1 (walk (c + 1) tyT2)
                TyRecord fieldtys -> TyRecord (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)
                TyVariant fieldtys -> TyVariant (map (\(li, ftys) -> (li, map (walk c) ftys)) fieldtys)

typeShiftAbove :: Int -> Int -> Type -> Type
typeShiftAbove d =
        tymap
                ( \c fi x ->
                        if x < c
                                then TyVar fi x
                                else TyVar fi (x + d)
                )

typeShift :: Int -> Type -> Type
typeShift d = typeShiftAbove d 0

typeSubst :: Type -> Int -> Type -> Type
typeSubst tyS =
        tymap
                ( \j fi x ->
                        if x == j
                                then typeShift j tyS
                                else TyVar fi x
                )

typeSubstTop :: Type -> Type -> Type
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Info -> Int -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar fi x -> onvar c fi x
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)
                TmLet fi t1 t2 -> TmLet fi (walk c t1) (walk (c + 1) t2)
                TmFix t1 -> TmFix (walk c t1)
                TmFold tyT -> TmFold (ontype c tyT)
                TmUnfold tyT -> TmUnfold (ontype c tyT)
                TmProj t1 l -> TmProj (walk c t1) l
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmTag l t1 tyT2 -> TmTag l (map (walk c) t1) (ontype c tyT2)
                TmCase t alts -> TmCase (walk c t) (map (\(fi, ti) -> (fi, walk c ti)) alts)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c fi x ->
                        if x < c
                                then TmVar fi x
                                else TmVar fi (x + d)
                )
                (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Term -> Int -> Term -> Term
termSubst s =
        tmmap
                ( \j fi x ->
                        if x == j
                                then termShift j s
                                else TmVar fi x
                )
                (\_ tyT -> tyT)

tytermSubst :: Type -> Int -> Term -> Term
tytermSubst tyS = tmmap (\_ x n -> TmVar x n) (typeSubst tyS)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst (termShift 1 s) 0 t)

tytermSubstTop :: Type -> Term -> Term
tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)