module Plato.Core.Evaluate where

import qualified Plato.Common.Name as N
import Plato.Core.Context
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
                TmTApp (TmTAbs x _ t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp t1' tyT2
                TmVar i n -> case getbinding ctx i n of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmLet x v1 t2 | isval v1 -> Just $ termSubstTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet x t1' t2
                TmCase v branches | isval v -> termSubstTop v <$> lookup v branches
                TmCase t1 branches -> do
                        t1' <- eval1 t1
                        Just $ TmCase t1' branches
                _ -> Nothing

{-
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: Context -> Int -> Maybe Ty
gettyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT _ -> Just tyT
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
                (TyAll tyX1 knK1 tyS2, TyAll _ knK2 tyT2) -> do
                        let ctx' = addbinding tyX1 NameBind `execState` ctx
                        knK1 == knK2 && tyeqv ctx' tyS2 tyT2
                (TyString, TyString) -> True
                (TyFloat, TyFloat) -> True
                (TyVariant fields1, TyVariant fields2) -> (length fields1 == length fields2) && fseqv fields1 fields2
                    where
                        fseqv [] [] = True
                        fseqv ((li1, tyTi1) : f1) ((li2, tyTi2) : f2) = (li1 == li2) && tyeqvs ctx tyTi1 tyTi2 && fseqv f1 f2
                        fseqv _ _ = False
                        tyeqvs ctx [] [] = True
                        tyeqvs ctx (tyS : tySs) (tyT : tyTs) = tyeqv ctx tyS tyT && tyeqvs ctx tySs tyTs
                        tyeqvs ctx _ _ = error "field length not match"
                _ -> False

getkind :: Context -> Int -> Kind
getkind ctx i = case getbinding ctx i of
        TyVarBind knK -> knK
        TyAbbBind _ (Just knK) -> knK
        TyAbbBind _ Nothing -> error $ "No kind recorded for variable " ++ N.name2str (index2name ctx i)
        _ -> error $ "getkind: Wrong kind of binding for variable " ++ N.name2str (index2name ctx i)

kindof :: Ty -> State Context Kind
kindof tyT = case tyT of
        TyArr tyT1 tyT2 -> do
                knK1 <- kindof tyT1
                knK2 <- kindof tyT2
                unless (knK1 == KnStar && knK2 == KnStar) (error "star kind expected")
                return KnStar
        TyVar i _ -> do
                ctx <- get
                let knK = getkind ctx i
                return knK
        TyAbs tyX knK1 tyT2 -> do
                addbinding tyX (TyVarBind knK1)
                knK2 <- kindof tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof tyT1
                knK2 <- kindof tyT2
                case knK1 of
                        KnArr knK11 knK12 ->
                                if knK2 == knK11
                                        then return knK12
                                        else error "parameter kind mismatch"
                        _ -> error "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                addbinding tyX (TyVarBind knK1)
                knK2 <- kindof tyT2
                when (knK2 /= KnStar) (error "Kind * expected")
                return KnStar
        _ -> return KnStar

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
        TmTAbs tyX knK1 t2 -> do
                addbinding tyX (TyVarBind knK1)
                tyT2 <- typeof t2
                return $ TyAll tyX knK1 tyT2
        TmTApp t1 tyT2 -> do
                knKT2 <- kindof tyT2
                tyT1 <- typeof t1
                ctx <- get
                case tyT1 of
                        TyAll _ knK11 tyT12 ->
                                if knK11 == knKT2
                                        then return $ typeSubstTop tyT2 tyT12
                                        else error "Type argument has wrong kind"
                        _ -> error "universal type expected"

evalbinding :: Context -> Binding -> Binding
evalbinding ctx b = case b of
        TmAbbBind t tyT -> let t' = eval ctx t in TmAbbBind t' tyT
        bind -> bind
-}