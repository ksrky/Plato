{-# LANGUAGE OverloadedStrings #-}

module Plato.Core.Check where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Core.Pretty
import Plato.Core.Subst
import Plato.Syntax.Core

import Control.Exception.Safe
import Control.Monad
import Prettyprinter

----------------------------------------------------------------
-- Type equality check
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind{} -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> unLoc tyT
        _ -> unreachable ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyApp (TyAbs _ _ tyT12) tyT2 -> Just $ typeSubstTop tyT2 tyT12
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp tyT1 tyT2 -> TyApp (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT' of
                Just tyT'' -> simplifyty ctx tyT''
                Nothing -> tyT'

tyeqv :: MonadThrow m => Context -> Ty -> Ty -> m ()
tyeqv ctx = tyeqv'
    where
        tyeqv' :: MonadThrow m => Ty -> Ty -> m ()
        tyeqv' tyS tyT = do
                let tyS' = simplifyty ctx tyS
                    tyT' = simplifyty ctx tyT
                case (tyS', tyT') of
                        (TyVar i _, _) | istyabb ctx i -> tyeqv' (gettyabb ctx i) tyT'
                        (_, TyVar i _) | istyabb ctx i -> tyeqv' tyS' (gettyabb ctx i)
                        (TyVar i _, TyVar j _) | i == j -> return ()
                        (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyAll tyX1 _ tyS2, TyAll _ _ tyT2) -> do
                                ctx' <- addName tyX1 ctx
                                tyeqv ctx' tyS2 tyT2
                        (TyAbs tyX1 knKS1 tyS2, TyAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                                ctx' <- addName tyX1 ctx
                                tyeqv ctx' tyS2 tyT2
                        (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyRec x1 _ tyS2, TyRec _ _ tyT2) -> do
                                ctx' <- addName x1 ctx
                                tyeqv ctx' tyS2 tyT2
                        (TyRecord fields1, TyRecord fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> tyeqv ctx tyTi1 tyTi2
                                        Nothing -> throwError $ hsep ["label", pretty li1, "not found"]
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> tyeqv ctx tyTi1 tyTi2
                                        Nothing -> throwError $ hsep ["label", pretty li2, "not found"]
                        (TyRecord _, TyRecord _) -> throwError "Record field lengths are not equal"
                        (TyVariant fields1, TyVariant fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> zipWithM (tyeqv ctx) tyTi1 tyTi2
                                        Nothing -> throwError $ hsep ["label", pretty li1, "not found."]
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> zipWithM (tyeqv ctx) tyTi1 tyTi2
                                        Nothing -> throwError $ hsep ["label", pretty li2, "not found"]
                        (TyVariant _, TyVariant _) -> throwError "Variant field lengths are not equal"
                        _ -> throwError $ hsep ["type mismatch:", ppr ctx tyS' <> comma, ppr ctx tyT']

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
getkind :: MonadThrow m => Context -> Int -> m Kind
getkind ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind tyT knK -> do
                knK' <- kindof ctx (unLoc tyT)
                unless (knK /= knK') $ throwError "Kind attachment failed"
                return knK
        _ -> throwError $ hsep ["getkind: Wrong kind of binding for variable", pretty (index2name ctx i)]

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyArr tyT1 tyT2 -> do
                checkKindStar ctx tyT1
                checkKindStar ctx tyT2
                return KnStar
        TyVar i _ -> getkind ctx i
        TyAbs tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr{} -> throwError "parameter kind mismatch"
                        _ -> throwError "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                checkKindStar ctx' tyT2
                return KnStar
        TyRec tyX knK1 tyT2 -> do
                ctx' <- addBinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                unless (knK1 == knK2) $ throwError $ hsep ["Kind", ppr ctx' knK1, "expected"]
                return KnStar
        TyRecord fieldtys -> do
                forM_ fieldtys $ \(_, tyS) -> checkKindStar ctx tyS
                return KnStar
        TyVariant fieldtys -> do
                forM_ fieldtys $ \(_, tys) -> do
                        forM_ tys $ \tyS -> checkKindStar ctx tyS
                return KnStar

checkKindStar :: MonadThrow m => Context -> Ty -> m ()
checkKindStar ctx tyT = do
        k <- kindof ctx tyT
        if k == KnStar
                then return ()
                else throwError $ hsep ["Kind * expected:", ppr ctx tyT]

typeof :: (MonadThrow m, MonadFail m) => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar i _ -> unLoc <$> getTypeFromContext NoSpan ctx i
        TmAbs x tyT1 t2 -> do
                checkKindStar ctx tyT1
                ctx' <- addBinding x (VarBind $ noLoc tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT2 tyT11
                                return tyT12
                        _ -> throwError "arrow type expected"
        TmTAbs tyX t2 -> do
                ctx' <- addBinding tyX (TyVarBind undefined) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll tyX undefined tyT2
        TmTApp t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ knK11 tyT12
                                | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                                | otherwise -> throwError "Type argument has wrong kind"
                        _ -> throwError "universal type expected"
        TmLet x t1 t2 -> do
                tyT1 <- typeof ctx t1
                ctx' <- addBinding x (VarBind $ noLoc tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ typeShift (-1) tyT2
        TmFix t1 -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv ctx tyT12 tyT11
                                return tyT12
                        _ -> throwError "arrow type expected"
        TmFold tyS -> case simplifyty ctx tyS of
                TyRec _ _ tyT -> return $ TyArr (typeSubstTop tyS tyT) tyS
                _ -> throwError "recursive type expected"
        TmUnfold tyS -> case simplifyty ctx tyS of
                TyRec _ _ tyT -> return $ TyArr tyS (typeSubstTop tyS tyT)
                _ -> throwError "recursive type expected"
        TmProj t1 l -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyRecord fieldtys -> case lookup l fieldtys of
                                Just tyT -> return tyT
                                Nothing -> throwError $ hsep ["label", pretty l, "not found in", ppr ctx tyT1]
                        _ -> throwError "Expected record type"
        TmRecord fields -> do
                fieldtys <- forM fields $ \(li, ti) -> do
                        tyTi <- typeof ctx ti
                        return (li, tyTi)
                return $ TyRecord fieldtys
        TmTag li ts1 tyT2 -> case simplifyty ctx tyT2 of
                TyVariant fieldtys -> case lookup li fieldtys of
                        Just tyTiExpected -> do
                                tyTi <- mapM (typeof ctx) ts1
                                mapM_ (uncurry $ tyeqv ctx) (zip tyTi tyTiExpected)
                                return tyT2
                        Nothing -> throwError $ hsep ["label", pretty li, "not found"]
                tyT2' -> throwError $ hsep ["Expected variant type, but got ", ppr ctx tyT2']
        TmCase t alts -> do
                tyT <- typeof ctx t
                undefined

{-case simplifyty ctx tyT of
        TyVariant fieldtys -> do
                when (null fieldtys) $ return ()
                (tyT1 : restTy) <- forM alts $ \(li, (ki, ti)) -> case lookup li fieldtys of
                        Just tys -> do
                                let ctx' = foldr addFreshName ctx (VarBind tyT)
                                tyTi <- typeof ctx' ti
                                return $ typeShift (- ki) tyTi
                        Nothing | null (g_name (nameText li)) -> do
                                let ctx' = foldr addFreshName ctx (VarBind $ TyVariant fieldtys)
                                tyTi <- typeof ctx' ti
                                return $ typeShift (- ki) tyTi
                        Nothing -> throwError $ hsep ["label", pretty li, "not found"]
                forM_ restTy $ \tyTi -> tyeqv ctx tyTi tyT1
                return tyT1
        tyT' -> throwError $ hsep ["Expected variant type, but got", ppr ctx tyT']-}

----------------------------------------------------------------
-- Type check of binding
----------------------------------------------------------------
checkBinding :: (MonadThrow m, MonadFail m) => Context -> Binding -> m ()
checkBinding ctx (TyAbbBind (L _ tyT) knK) = do
        knK' <- kindof ctx tyT
        unless (knK == knK') $ throwError "Kind of binding does not match declared kind"
checkBinding ctx (TmAbbBind (L _ t) (L _ tyT)) = do
        tyT' <- typeof ctx t
        tyeqv ctx tyT tyT'
checkBinding _ _ = return ()
