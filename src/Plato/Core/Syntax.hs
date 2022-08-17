{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Syntax where

import Plato.Common.Info
import Plato.Common.Name

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Ty
        = TyVar Info Int Int
        | TyArr Info Ty Ty
        | TyAll Info Name Kind Ty
        | TyAbs Info Name Kind Ty
        | TyApp Info Ty Ty
        | TyRecord Info [(Name, Ty)]
        | TyVariant [(Name, [Ty])]
        deriving (Eq, Show)

data Term
        = TmVar Info Int Int
        | TmAbs Info Name Ty Term
        | TmApp Info Term Term
        | TmTAbs Info Name Kind Term
        | TmTApp Info Term Ty
        | TmLet Info Name Term Term
        | TmFix Info Term
        | TmProj Info Term Name
        | TmRecord Info [(Name, Term)]
        | TmTag Info Name [Term] Ty
        | TmCase Info Term [(Name, (Int, Term))]
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Ty
        | TyVarBind Kind
        | TyAbbBind Ty (Maybe Kind)
        | TmAbbBind Term (Maybe Ty)
        deriving (Eq, Show)

newtype Import = Import ModuleName deriving (Eq, Show)

data Commands = Commands {imports :: [Import], binds :: [(Name, Binding)], body :: Term} deriving (Eq, Show)

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
tymap :: (Info -> Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar fi x n -> onvar fi c x n
                TyArr fi tyT1 tyT2 -> TyArr fi (walk c tyT1) (walk c tyT2)
                TyAll fi tyX knK1 tyT2 -> TyAll fi tyX knK1 (walk (c + 1) tyT2)
                TyAbs fi tyX knK1 tyT2 -> TyAbs fi tyX knK1 (walk (c + 1) tyT2)
                TyApp fi tyT1 tyT2 -> TyApp fi (walk c tyT1) (walk c tyT2)
                TyRecord fi fieldtys -> TyRecord fi (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)
                TyVariant fieldtys -> TyVariant (map (\(li, tyTi) -> (li, map (walk c) tyTi)) fieldtys)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                ( \fi c x n ->
                        if x < c
                                then TyVar fi x (n + d)
                                else TyVar fi (x + d) (n + d)
                )

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
        tymap
                ( \fi j x n ->
                        if x == j
                                then typeShift j tyS
                                else TyVar fi x n
                )

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Info -> Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar fi x n -> onvar fi c x n
                TmAbs fi x tyT1 t2 -> TmAbs fi x (ontype c tyT1) (walk (c + 1) t2)
                TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)
                TmTAbs fi tyX knK1 t2 -> TmTAbs fi tyX knK1 (walk (c + 1) t2)
                TmTApp fi t1 tyT2 -> TmTApp fi (walk c t1) (ontype c tyT2)
                TmFix fi t1 -> TmFix fi (walk c t1)
                TmProj fi t1 l -> TmProj fi (walk c t1) l
                TmRecord fi fields -> TmRecord fi (map (\(li, ti) -> (li, walk c ti)) fields)
                TmLet fi x t1 t2 -> TmLet fi x (walk c t1) (walk (c + 1) t2)
                TmTag fi l t1 tyT2 -> TmTag fi l (map (walk c) t1) (ontype c tyT2)
                TmCase fi t alts -> TmCase fi (walk c t) (map (\(li, (ki, ti)) -> (li, (ki, walk (c + ki) ti))) alts)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \fi c x n ->
                        if x < c
                                then TmVar fi x (n + d)
                                else TmVar fi (x + d) (n + d)
                )
                (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                ( \fi j x n ->
                        if x == j
                                then termShift j s
                                else TmVar fi x n
                )
                (\j tyT -> tyT)
                j

tytermSubst :: Ty -> Int -> Term -> Term
tytermSubst tyS = tmmap (\fi c x n -> TmVar fi x n) (typeSubst tyS)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

tytermSubstTop :: Ty -> Term -> Term
tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
