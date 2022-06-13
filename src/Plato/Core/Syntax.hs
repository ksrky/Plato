module Plato.Core.Syntax where

import Control.Monad.State

data Term
        = TmVar Int Int
        | TmAbs String Ty Term
        | TmApp Term Term
        | TmTAbs String Term
        | TmTApp Term Ty
        deriving (Eq, Show)

data Ty
        = TyVar Int Int
        | TyArr Ty Ty
        | TyAll String Ty
        deriving (Eq, Show)

data Binding = NameBind | VarBind Ty | TyVarBind

type Context = [(String, Binding)]

----------------------------------------------------------------
-- Context
----------------------------------------------------------------
addbinding :: String -> Binding -> State Context ()
addbinding x bind = modify $ \ctx -> (x, bind) : ctx

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case ctx !! i of
        (_, VarBind tyT) -> tyT
        _ -> error $ "getTypeFromContext: Wrong kind of binding for variable " ++ index2name ctx i

pickfreshname :: Monad m => String -> StateT Context m String
pickfreshname x = state $ \ctx -> case lookup x ctx of
        Just _ -> pickfreshname (x ++ "'") `runState` ctx
        Nothing -> (x, (x, NameBind) : ctx)

index2name :: Context -> Int -> String
index2name ctx x = fst (ctx !! x)

----------------------------------------------------------------
-- Type
----------------------------------------------------------------
typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d =
        tymap
                ( \c x n -> case () of
                        _ | x < c -> TyVar x (n + d)
                        _ | x + d < 0 -> error "Scoping error!"
                        _ -> TyVar (x + d) (n + d)
                )

tymap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tymap onvar c tyT = walk c tyT
    where
        walk c tyT = case tyT of
                TyArr tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
                TyVar x n -> onvar c x n
                TyAll tyX tyT2 -> TyAll tyX (walk (c + 1) tyT2)

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
----------------------------------------------------------------z
tmmap :: (Int -> Int -> Int -> Term) -> (Int -> Ty -> Ty) -> Int -> Term -> Term
tmmap onvar ontype c t = walk c t
    where
        walk c t = case t of
                TmVar x n -> onvar c x n
                TmAbs x tyT1 t2 -> TmAbs x (ontype c tyT1) (walk (c + 1) t2)
                TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
                TmTAbs tyX t2 -> TmTAbs tyX (walk (c + 1) t2)
                TmTApp t1 tyT2 -> TmTApp (walk c t1) (ontype c tyT2)

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
