{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Syntax where

import qualified Plato.Common.Name as N
import qualified Plato.Core.Error as E

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyString
        | TyFloat
        | TyVariant [(N.Name, [Ty])]
        | TyRec N.Name Ty
        | TyAbs N.Name Kind Ty
        | TyArr Ty Ty
        | TyApp Ty Ty
        | TyAll N.Name Kind Ty
        deriving (Eq, Show)

data Term
        = TmVar Int Int
        | TmAbs N.Name Ty Term
        | TmApp Term Term
        | TmTAbs N.Name Kind Term
        | TmTApp Term Ty
        | TmFloat Float
        | TmString String
        | TmLet N.Name Term Term
        | TmCase Term [(N.Name, Term)]
        | TmTag N.Name [Term] Ty
        | TmFold Ty
        | TmUnfold Ty
        deriving (Eq, Show)

data Binding
        = NameBind
        | VarBind Ty
        | TyVarBind Kind
        | TyAbbBind Ty (Maybe Kind)
        | TmAbbBind Term (Maybe Ty)
        deriving (Eq, Show)

data Command
        = Import N.Name
        | Bind N.Name Binding
        | Eval Term
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
                TyAbs tyX knK1 tyT2 -> TyAbs tyX knK1 (walk (c + 1) tyT2)
                TyAll tyX knK1 tyT2 -> TyAll tyX knK1 (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyVariant fieldtys -> TyVariant (map (\(li, tyTi) -> (li, map (walk c) tyTi)) fieldtys)
                TyRec x tyT -> TyRec x (walk (c + 1) tyT)
                TyFloat -> TyFloat
                TyString -> TyString

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                ( \c x n -> case () of
                        _ | x < c -> TyVar x (n + d)
                        _ -> TyVar (x + d) (n + d)
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
                TmLet x t1 t2 -> TmLet x (walk c t1) (walk (c + 1) t2)
                TmCase t alts -> TmCase (walk c t) (map (\(li, ti) -> (li, walk (c + 1) ti)) alts)
                TmTag l t1 tyT -> TmTag l (map (walk c) t1) (ontype c tyT)
                TmTAbs tyX knK1 t2 -> TmTAbs tyX knK1 (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)
                TmFold tyT -> TmFold (ontype c tyT)
                TmUnfold tyT -> TmUnfold (ontype c tyT)
                _ -> t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \c x n ->
                        if x >= c
                                then TmVar (x + d) (n + d)
                                else TmVar x (n + d)
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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------
getTyArr :: Ty -> [Ty]
getTyArr tyT1 = case tyT1 of
        TyArr tyT11 tyT12 -> tyT11 : getTyArr tyT12
        _ -> []