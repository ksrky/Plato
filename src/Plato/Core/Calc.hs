{-# LANGUAGE MultiParamTypeClasses #-}

module Plato.Core.Calc where

import Plato.Syntax.Core

tmmap :: (Int -> Int -> Info -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x fi -> onvar c x fi
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)
                TmLet fi t1 t2 -> TmLet fi (walk c t1) (walk (c + 1) t2)
                TmFix t1 -> TmFix (walk c t1)
                TmProj t1 l -> TmProj (walk c t1) l
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmCon l ts -> TmCon l (map (walk c) ts)

tymap :: (Int -> Int -> Info -> Type) -> Int -> Type -> Type
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar fi x -> onvar c fi x
                TyFun tyT1 tyT2 -> TyFun (walk c tyT1) (walk c tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyRecord fieldtys -> TyRecord (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)

class Shifting a where
        shiftAbove :: Int -> Int -> a -> a
        shift :: Int -> a -> a
        shift d = shiftAbove d 0

instance Shifting Term where
        shiftAbove d =
                tmmap
                        ( \c x fi ->
                                if x < c
                                        then TmVar x fi
                                        else TmVar (x + d) fi
                        )
                        (shiftAbove d)

instance Shifting Type where
        shiftAbove d =
                tymap
                        ( \c x fi ->
                                if x < c
                                        then TyVar x fi
                                        else TyVar (x + d) fi
                        )

class Substitution a b where
        subst :: a -> Int -> b -> b
        substTop :: a -> b -> b
        (|->) :: a -> b -> b
        (|->) = substTop

instance Substitution Term Term where
        subst t =
                tmmap
                        ( \j x fi ->
                                if x == j
                                        then shift j t
                                        else TmVar x fi
                        )
                        (\_ tyT -> tyT)
        substTop s t = shift (-1) (subst (shift 1 s) 0 t)

instance Substitution Type Type where
        subst tyS =
                tymap
                        ( \j x fi ->
                                if x == j
                                        then shift j tyS
                                        else TyVar x fi
                        )
        substTop tyS tyT = shift (-1) (subst (shift 1 tyS) 0 tyT)

instance Substitution Type Term where
        subst tyS = tmmap (\_ x fi -> TmVar x fi) (subst tyS)
        substTop tyS t = shift (-1) (subst (shift 1 tyS) 0 t)