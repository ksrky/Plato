{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Syntax where

import qualified Plato.Common.Error as E
import Plato.Common.Info
import qualified Plato.Common.Name as N

data Kind = KnStar | KnArr Kind Kind deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyString
        | TyFloat
        | TyVariant [(N.Name, [Ty])]
        | TyAbs (N.Name, Kind) Ty
        | TyArr Ty Ty
        | TyApp Ty Ty
        | TyAll (N.Name, Kind) Ty
        deriving (Eq, Show)

data Term
        = TmVar Info Int Int
        | TmAbs Info (N.Name, Ty) Term
        | TmApp Info Term Term
        | TmTAbs Info (N.Name, Kind) Term
        | TmTApp Info Term Ty
        | TmFloat Info Float
        | TmString Info String
        | TmLet Info (N.Name, Term) Term
        | TmCase Info Term [(N.Name, (Int, Term))]
        | TmTag Info N.Name [Term] Ty
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
                TyAbs (tyX, knK1) tyT2 -> TyAbs (tyX, knK1) (walk (c + 1) tyT2)
                TyAll (tyX, knK1) tyT2 -> TyAll (tyX, knK1) (walk (c + 1) tyT2)
                TyApp tyT1 tyT2 -> TyApp (walk c tyT1) (walk c tyT2)
                TyVariant fields -> TyVariant (map (\(li, tyTi) -> (li, map (walk c) tyTi)) fields)
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
tmmap :: (Info -> Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar fi x n -> onvar fi c x n
                TmAbs fi (x, tyT1) t2 -> TmAbs fi (x, ontype c tyT1) (walk (c + 1) t2)
                TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)
                TmLet fi (x, t1) t2 -> TmLet fi (x, walk c t1) (walk (c + 1) t2)
                TmCase fi t alts -> TmCase fi (walk c t) (map (\(li, (ki, ti)) -> (li, (ki, walk (c + ki) ti))) alts)
                TmTag fi l t1 tyT -> TmTag fi l (map (walk c) t1) (ontype c tyT)
                TmTAbs fi (tyX, knK1) t2 -> TmTAbs fi (tyX, knK1) (walk (c + 1) t2)
                TmTApp fi t1 tyT2 -> TmTApp fi (walk c t1) (ontype c tyT2)
                _ -> t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
        tmmap
                ( \fi c x n ->
                        if x >= c
                                then TmVar fi (x + d) (n + d)
                                else TmVar fi x (n + d)
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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------
countTyArr :: Ty -> Int
countTyArr tyT1 = case tyT1 of
        TyArr _ tyT12 -> 1 + countTyArr tyT12
        _ -> 0
