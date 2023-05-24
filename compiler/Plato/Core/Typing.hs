module Plato.Core.Typing where

import Control.Monad.Reader
import Plato.Common.Error
import Plato.Common.Utils
import Plato.Core.Calc
import Plato.Core.Env
import Plato.Core.Monad
import Plato.Syntax.Core

----------------------------------------------------------------
-- Type equivalence
----------------------------------------------------------------
istyabb :: CoreEnv -> Int -> Bool
istyabb env i = case getBinding i env of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: CoreEnv -> Int -> Type
gettyabb env i = case getBinding i env of
        TyAbbBind tyT _ -> tyT
        _ -> unreachable "gettyabb"

computety :: CoreEnv -> Type -> Maybe Type
computety ctx tyT = case tyT of
        TyApp (TyAbs _ _ tyT12) tyT2 -> Just $ tyT2 |-> tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: CoreEnv -> Type -> Type
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT' of
                Just tyT' -> simplifyty ctx tyT'
                Nothing -> tyT

tyeqv :: CoreMonad env m => Type -> Type -> m ()
tyeqv tyS tyT = do
        env <- asks getEnv
        let tyS' = simplifyty env tyS
            tyT' = simplifyty env tyT
        case (tyS', tyT') of
                (TyVar i _, TyVar j _) | i == j -> return ()
                (TyVar i _, _) | istyabb env i -> tyeqv (gettyabb env i) tyT
                (_, TyVar i _) | istyabb env i -> tyeqv tyS (gettyabb env i)
                (TyFun tyS1 tyS2, TyFun tyT1 tyT2) -> do
                        tyeqv tyS1 tyT1
                        tyeqv tyS2 tyT2
                (TyAll tyX1 _ tyS2, TyAll _ _ tyT2) -> do
                        extendNameWith (actualName tyX1) $ tyeqv tyS2 tyT2
                (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> do
                        tyeqv tyS1 tyT1
                        tyeqv tyS2 tyT2
                (TyAbs tyX1 knKS1 tyS2, TyAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                        extendNameWith (actualName tyX1) $ tyeqv tyS2 tyT2
                (TyRec tyX1 knKS1 tyS2, TyRec _ knKT1 tyT2) | knKS1 == knKT1 -> do
                        extendNameWith (actualName tyX1) $ tyeqv tyS2 tyT2
                (TyRecord fields1, TyRecord fields2) | length fields1 == length fields2 -> do
                        zipWithM_ (\(_, tyS) (_, tyT) -> tyeqv tyS tyT) fields1 fields2
                (TySum fields1, TySum fields2) -> do
                        zipWithM_ tyeqv fields1 fields2
                _ -> fail "type mismatch: " -- ++ pretty env tyS ++ ", " ++ printty env tyT

----------------------------------------------------------------
-- Typing
----------------------------------------------------------------
typeof :: CoreMonad env m => Term -> m Type
typeof t = case t of
        TmVar i _ -> getType i
        TmApp t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- typeof t2
                case tyT1 of
                        TyFun tyT11 tyT12 -> do
                                tyeqv tyT2 tyT11
                                return tyT12
                        _ -> fail "arrow type expected"
        TmAbs x tyT1 t2 -> do
                checkKnStar tyT1
                tyT2 <- extendWith (actualName x) (TmVarBind tyT1) $ typeof t2
                return $ TyFun tyT1 (shift (-1) tyT2)
        TmTApp t1 tyT2 -> do
                knKT2 <- kindof tyT2
                tyT1 <- typeof t1
                env <- asks getEnv
                case simplifyty env tyT1 of
                        TyAll _ knK11 tyT12
                                | knK11 == knKT2 -> return $ tyT2 |-> tyT12
                                | otherwise -> fail "Type argument has wrong kind"
                        _ -> fail "universal type expected"
        TmTAbs tyX knK1 t2 -> do
                tyT2 <- extendWith (actualName tyX) (TyVarBind knK1) $ typeof t2
                return $ TyAll tyX knK1 tyT2
        TmLet x t1 t2 -> do
                tyT1 <- typeof t1
                tyT2 <- extendWith (actualName x) (TmVarBind tyT1) $ typeof t2
                return $ shift (-1) tyT2
        TmFix t1 -> do
                tyT1 <- typeof t1
                env <- asks getEnv
                case simplifyty env tyT1 of
                        TyFun tyT11 tyT12 -> do
                                tyeqv tyT12 tyT11
                                return tyT12
                        _ -> fail "arrow type expected"
        TmProj t1 i -> do
                tyT1 <- typeof t1
                env <- asks getEnv
                case simplifyty env tyT1 of
                        TyRecord fieldtys -> case safeAt fieldtys i of
                                Just (_, tyT) -> return tyT
                                Nothing -> fail "label not found"
                        _ -> fail "record type expected"
        TmRecord fields -> do
                fieldtys <- forM fields $ \(li, ti) -> do
                        tyTi <- typeof ti
                        return (li, tyTi)
                return $ TyRecord fieldtys
        TmInj i t1 tyT2 -> do
                env <- asks getEnv
                case simplifyty env tyT2 of
                        TySum fields -> case safeAt fields i of
                                Just tyTi -> do
                                        tyT1 <- typeof t1
                                        tyeqv tyT1 tyTi
                                        return tyT2
                                Nothing -> fail "label not found"
                        _ -> fail "sum type expected"

----------------------------------------------------------------
-- Kinding
----------------------------------------------------------------
checkKnStar :: CoreMonad env m => Type -> m ()
checkKnStar tyT = do
        knK <- kindof tyT
        case knK of
                KnStar -> return ()
                _ -> fail "Kind * expected"

kindof :: CoreMonad env m => Type -> m Kind
kindof tyT = case tyT of
        TyVar i _ -> getKind i
        TyFun tyT1 tyT2 -> do
                checkKnStar tyT1
                checkKnStar tyT2
                return KnStar
        TyAbs tyX knK1 tyT2 -> do
                knK2 <- extendWith (actualName tyX) (TyVarBind knK1) $ kindof tyT2
                return $ KnFun knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof tyT1
                knK2 <- kindof tyT2
                case knK1 of
                        KnFun knK11 knK12
                                | knK2 == knK11 -> return knK12
                                | otherwise -> fail "parameter kind mismatch"
                        _ -> fail "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                extendWith (actualName tyX) (TyVarBind knK1) $ checkKnStar tyT2
                return KnStar
        TyRec tyX knK1 tyT2 -> extendWith (actualName tyX) (TyVarBind knK1) $ kindof tyT2
        TyRecord fields -> do
                mapM_ (\(_, tyTi) -> checkKnStar tyTi) fields
                return KnStar
        TySum fields -> do
                mapM_ checkKnStar fields
                return KnStar
