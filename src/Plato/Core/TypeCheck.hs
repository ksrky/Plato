module Plato.Core.TypeCheck where

import Plato.Core.Syntax

import Control.Monad.State

isval :: Term -> Bool
isval t = case t of
        TmString{} -> True
        TmFloat{} -> True
        TmAbs{} -> True
        TmTAbs{} -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmApp (TmAbs x _ t12) v2 | isval v2 -> return $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        return $ TmApp t1' t2
                TmTApp (TmTAbs x t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp t1' tyT2
                TmVar n _ -> case getbinding ctx n of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmLet x v1 t2 | isval v1 -> Just $ termSubstTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet x t1' t2
                TmCase v branches | isval v -> termSubstTop v <$> lookup v branches
                _ -> Nothing

istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT -> True
        _ -> False

gettyabb :: Context -> Int -> Maybe Ty
gettyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT -> Just tyT
        _ -> Nothing

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyVar i _ | istyabb ctx i -> gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT = case computety ctx tyT of
        Just tyT' -> simplifyty ctx tyT'
        Nothing -> tyT

tyeqv :: Context -> Ty -> Ty -> Bool
tyeqv ctx tyS tyT = do
        let tyS = simplifyty ctx tyS
        let tyT = simplifyty ctx tyT
        case (tyS, tyT) of
                (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> tyeqv ctx tyS1 tyT1 && tyeqv ctx tyS2 tyT2
                (TyVar i _, _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx ty tyT
                        Nothing -> False
                (_, TyVar i _) | istyabb ctx i -> case gettyabb ctx i of
                        Just ty -> tyeqv ctx tyS ty
                        Nothing -> False
                (TyVar i _, TyVar j _) -> i == j
                (TyAll tyX1 tyS2, TyAll _ tyT2) -> do
                        let ctx' = addbinding tyX1 NameBind `execState` ctx
                        tyeqv ctx' tyS2 tyT2
                (TyString, TyString) -> True
                (TyFloat, TyFloat) -> True
                _ -> False

typeof :: Term -> State Context Ty
typeof t = case t of
        TmString _ -> return TyString
        TmFloat _ -> return TyFloat
        TmVar i _ -> do
                ctx <- get
                return $ getTypeFromContext ctx i
        TmAbs x tyT1 t2 -> do
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyArr tyT11 tyT12 | tyT2 == tyT11 -> return tyT12
                        TyArr tyT11 tyT12 -> error "parameter type mismatch"
                        _ -> error "arrow type expected"
        TmLet x t1 t2 -> do
                tyT1 <- typeof t1
                addbinding x (VarBind tyT1)
                tyT2 <- typeof t2
                return $ typeShift (-1) tyT2
        TmCase{} -> undefined
        TmTAbs tyX t2 -> do
                addbinding tyX TyVarBind
                tyT2 <- typeof t2
                return $ TyAll tyX tyT2
        TmTApp t1 tyT2 -> do
                tyT1 <- typeof t1
                case tyT1 of
                        TyAll _ tyT12 -> return $ typeSubstTop tyT2 tyT12
                        _ -> error "universal type expected"

evalbinding :: Context -> Binding -> Binding
evalbinding ctx b = case b of
        TmAbbBind t tyT -> let t' = eval ctx t in TmAbbBind t' tyT
        bind -> bind
