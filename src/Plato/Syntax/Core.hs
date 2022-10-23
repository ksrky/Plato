module Plato.Syntax.Core where

import Plato.Common.Name
import Plato.Common.SrcLoc

----------------------------------------------------------------
-- Syntax
----------------------------------------------------------------
data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAll Name Kind Ty
        | TyAbs Name Kind Ty
        | TyApp Ty Ty
        | TyRec Name Kind Ty
        | TyId Name
        | TyRecord [(Name, Ty)]
        | TyVariant [(Name, [Ty])]
        deriving (Eq, Show)

data Term
        = TmVar Int Int
        | TmAbs Name Ty Term
        | TmApp Term Term
        | TmTAbs Name Kind Term
        | TmTApp Term Ty
        | TmLet Name Term Term
        | TmFix Term
        | TmFold Ty
        | TmUnfold Ty
        | TmProj Term Name
        | TmRecord [(Name, Term)]
        | TmTag Name [Term] Ty
        | TmCase Term [(Name, (Int, Term))]
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind (Located Ty)
        | TyVarBind Kind
        | TmAbbBind (Located Term) (Maybe (Located Ty))
        | TyAbbBind (Located Ty) (Maybe Kind)
        deriving (Eq, Show)

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyVar x n -> onvar c x n
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyRec tyX knK1 tyT2 -> TyRec tyX knK1 (walk (c + 1) tyT)
                TyId b -> TyId b
                TyRecord fieldtys -> TyRecord (map (\(li, tyTi) -> (li, walk c tyTi)) fieldtys)
                TyVariant fieldtys -> TyVariant (map (\(li, tyTi) -> (li, map (walk c) tyTi)) fieldtys)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                ( \c x n ->
                        if x < c
                                then TyVar x (n + d)
                                else TyVar (x + d) (n + d)
                )

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS =
        tymap
                ( \j x n ->
                        if x == j
                                then typeShift j tyS
                                else TyVar x n
                )

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

----------------------------------------------------------------
-- Term
----------------------------------------------------------------
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)
                TmFix t1 -> TmFix (walk c t1)
                TmFold tyT -> TmFold (ontype c tyT)
                TmUnfold tyT -> TmUnfold (ontype c tyT)
                TmProj t1 l -> TmProj (walk c t1) l
                TmRecord fields -> TmRecord (map (\(li, ti) -> (li, walk c ti)) fields)
                TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
                TmTag l t1 tyT2 -> TmTag l (map (walk c) t1) (ontype c tyT2)
                TmCase t alts -> TmCase (walk c t) (map (\(li, (ki, ti)) -> (li, (ki, walk (c + ki) ti))) alts)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c x n ->
                        if x < c
                                then TmVar x (n + d)
                                else TmVar (x + d) (n + d)
                )
                (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j s =
        tmmap
                ( \j x n ->
                        if x == j
                                then termShift j s
                                else TmVar x n
                )
                (\j tyT -> tyT)
                j

tytermSubst :: Ty -> Int -> Term -> Term
tytermSubst tyS = tmmap (\c x n -> TmVar x n) (typeSubst tyS)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

tytermSubstTop :: Ty -> Term -> Term
tytermSubstTop tyS t = termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)