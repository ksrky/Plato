module Plato.Core.Eval where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Core.Utils

import Control.Exception.Safe
import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmAbs{} -> True
        TmRecord _ fields -> all (\(l, ti) -> isval ctx ti) fields
        TmTag _ _ ts1 _ -> all (isval ctx) ts1
        TmApp _ (TmFold _ _) v -> isval ctx v
        TmTAbs{} -> True
        TmVar _ i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar _ i n -> case getBinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp _ (TmUnfold _ tyS) (TmApp _ (TmFold _ tyT) v) | isval ctx v -> Just v
                TmApp fi1 (TmFold fi2 tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp fi1 (TmFold fi2 tyS) t2'
                TmApp fi1 (TmUnfold fi2 tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp fi1 (TmUnfold fi2 tyS) t2'
                TmApp _ (TmAbs _ x _ t12) v2 | isval ctx v2 -> do
                        return $ termSubstTop v2 t12
                TmApp fi v1 t2 | isval ctx v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp fi v1 t2'
                TmApp fi v1 t2 | isval ctx v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp fi v1 t2'
                TmApp fi t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp fi t1' t2
                TmTApp _ (TmTAbs _ x _ t11) tyT2 -> Just $ tytermSubstTop tyT2 t11
                TmTApp fi t1 tyT2 -> do
                        t1' <- eval1 t1
                        Just $ TmTApp fi t1' tyT2
                TmLet _ x v1 t2 | isval ctx v1 -> Just $ termSubstTop v1 t2
                TmLet fi x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet fi x t1' t2
                TmFix fi v1 | isval ctx v1 -> case v1 of
                        TmAbs _ _ _ t12 -> Just $ termSubstTop t t12
                        _ -> Nothing
                TmFix fi t1 -> do
                        t1' <- eval1 t1
                        Just $ TmFix fi t1'
                TmProj fi (TmRecord fi2 fields) l -> lookup l fields
                TmProj fi t1 l -> do
                        t1' <- eval1 t1
                        Just $ TmProj fi t1' l
                TmRecord fi fields -> do
                        let evalafield :: [(Name, Term)] -> Maybe [(Name, Term)]
                            evalafield l = case l of
                                [] -> Nothing
                                (l, vi) : rest | isval ctx vi -> do
                                        rest' <- evalafield rest
                                        Just $ (l, vi) : rest'
                                (l, ti) : rest -> do
                                        ti' <- eval1 ti
                                        Just $ (l, ti') : rest
                        fields' <- evalafield fields
                        Just $ TmRecord fi fields'
                TmTag _ l vs tyT | all (isval ctx) vs -> Nothing
                TmTag fi l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag fi l ts' tyT
                TmCase _ (TmTag _ li vs11 _) alts | all (isval ctx) vs11 -> case lookup li alts of
                        Just (_, body) -> Just $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase fi t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase fi t1' alts
                _ -> Nothing

----------------------------------------------------------------
-- Type equality check
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getBinding ctx i of
        TyAbbBind tyT _ -> tyT
        _ -> error ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyApp _ (TyAbs _ _ _ tyT12) tyT2 -> Just $ typeSubstTop tyT2 tyT12
        TyVar _ i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT =
        let tyT' = case tyT of
                TyApp fi tyT1 tyT2 -> TyApp fi (simplifyty ctx tyT1) tyT2
                _ -> tyT
         in case computety ctx tyT' of
                Just tyT'' -> simplifyty ctx tyT''
                Nothing -> tyT'

tyeqv :: MonadThrow m => Info -> Context -> Ty -> Ty -> m ()
tyeqv fi ctx = tyeqv'
    where
        tyeqv' :: MonadThrow m => Ty -> Ty -> m ()
        tyeqv' tyS tyT = do
                let tyS' = simplifyty ctx tyS
                    tyT' = simplifyty ctx tyT
                case (tyS', tyT') of
                        (TyVar _ i _, _) | istyabb ctx i -> tyeqv' (gettyabb ctx i) tyT'
                        (_, TyVar _ i _) | istyabb ctx i -> tyeqv' tyS' (gettyabb ctx i)
                        (TyVar _ i _, TyVar _ j _) | i == j -> return ()
                        (TyArr _ tyS1 tyS2, TyArr _ tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyAll fi1 tyX1 _ tyS2, TyAll _ _ _ tyT2) -> do
                                ctx' <- addName fi1 tyX1 ctx
                                tyeqv fi1 ctx' tyS2 tyT2
                        (TyAbs fi1 tyX1 knKS1 tyS2, TyAbs _ _ knKT1 tyT2) | knKS1 == knKT1 -> do
                                ctx' <- addName fi1 tyX1 ctx
                                tyeqv fi1 ctx' tyS2 tyT2
                        {-(TyApp fi1 (TyId _ b1) tyS2, _) -> do
                                i <- getVarIndex fi ctx b1
                                tyeqv' (TyApp fi1 (gettyabb ctx i) tyS2) tyT'
                        (_, TyApp fi2 (TyId _ b2) tyT2) -> do
                                i <- getVarIndex fi ctx b2
                                tyeqv' tyS' (TyApp fi2 (gettyabb ctx i) tyT2)-}
                        (TyApp _ tyS1 tyS2, TyApp _ tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyRec fi1 x1 _ tyS2, TyRec _ _ _ tyT2) -> do
                                ctx' <- addName fi1 x1 ctx
                                tyeqv fi1 ctx tyS2 tyT2
                        (TyId _ b1, TyId _ b2) | b1 == b2 -> return ()
                        (TyId fi1 b1, _) -> do
                                i <- getVarIndex fi1 ctx b1
                                tyeqv' (gettyabb ctx i) tyT'
                        (_, TyId fi2 b2) -> do
                                i <- getVarIndex fi2 ctx b2
                                tyeqv' tyS' (gettyabb ctx i)
                        (TyRecord fi1 fields1, TyRecord fi2 fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> tyeqv fi1 ctx tyTi1 tyTi2
                                        Nothing -> throwError fi1 $ "label " ++ show li1 ++ " not found"
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> tyeqv fi2 ctx tyTi1 tyTi2
                                        Nothing -> throwError fi2 $ "label " ++ show li2 ++ " not found"
                        (TyRecord _ _, TyRecord fi2 _) -> throwError fi2 "Record field lengths are not equal"
                        (TyVariant fi1 fields1, TyVariant fi2 fields2) | length fields1 == length fields2 -> do
                                forM_ fields1 $ \(li1, tyTi1) -> case lookup li1 fields2 of
                                        Just tyTi2 -> zipWithM (tyeqv fi1 ctx) tyTi1 tyTi2
                                        Nothing -> throwError fi1 $ "label " ++ show li1 ++ " not found"
                                forM_ fields2 $ \(li2, tyTi2) -> case lookup li2 fields1 of
                                        Just tyTi1 -> zipWithM (tyeqv fi2 ctx) tyTi1 tyTi2
                                        Nothing -> throwError fi2 $ "label " ++ show li2 ++ " not found"
                        (TyVariant _ fields1, TyVariant fi2 fields2) -> throwError fi2 "Variant field lengths are not equal"
                        _ -> throwError fi $ "type mismatch: " ++ pretty (ctx, tyS') ++ ", " ++ pretty (ctx, tyT')

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
getkind :: MonadThrow m => Info -> Context -> Int -> m Kind
getkind fi ctx i = case getBinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind tyT (Just knK) -> do
                knK' <- kindof ctx tyT
                unless (knK /= knK') $ throwError fi "Kind attachment failed"
                return knK
        TyAbbBind tyT Nothing -> kindof ctx tyT
        _ -> throwError fi $ "getkind: Wrong kind of binding for variable " ++ pretty (index2name ctx i)

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyArr _ tyT1 tyT2 -> do
                checkKindStar (getInfo tyT1) ctx tyT1
                checkKindStar (getInfo tyT2) ctx tyT2
                return KnStar
        TyVar fi i _ -> getkind fi ctx i
        TyAbs fi tyX knK1 tyT2 -> do
                ctx' <- addBinding fi tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp fi tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr{} -> throwError (getInfo tyT1) "parameter kind mismatch"
                        _ -> throwError fi "arrow kind expected"
        TyAll fi tyX knK1 tyT2 -> do
                ctx' <- addBinding fi tyX (TyVarBind knK1) ctx
                checkKindStar (getInfo tyT2) ctx' tyT2
                return KnStar
        TyRec fi tyX knK1 tyT2 -> do
                ctx' <- addBinding fi tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                unless (knK1 == knK2) $ throwError (getInfo tyT2) $ "Kind " ++ show knK1 ++ " expected"
                return KnStar
        TyId fi b -> do
                -- i <- getVarIndex fi ctx b
                -- getkind fi ctx i -- todo: kind of recursive type stuck
                return KnStar
        TyRecord fi fieldtys -> do
                forM_ fieldtys $ \(l, tyS) -> checkKindStar fi ctx tyS
                return KnStar
        TyVariant fi fieldtys -> do
                forM_ fieldtys $ \(l, tys) -> do
                        forM_ tys $ \tyS -> checkKindStar fi ctx tyS
                return KnStar

checkKindStar :: MonadThrow m => Info -> Context -> Ty -> m ()
checkKindStar fi ctx tyT = do
        k <- kindof ctx tyT
        if k == KnStar
                then return ()
                else throwError fi $ "Kind * expected: " ++ pretty (ctx, tyT)

typeof :: (MonadThrow m, MonadFail m) => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar fi i _ -> getTypeFromContext fi ctx i
        TmAbs fi x tyT1 t2 -> do
                --checkKindStar fi ctx tyT1
                ctx' <- addBinding fi x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr dummyInfo tyT1 (typeShift (-1) tyT2)
        TmApp fi t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr _ tyT11 tyT12 -> do
                                tyeqv fi ctx tyT2 tyT11
                                return tyT12
                        _ -> throwError (getInfo tyT1) "arrow type expected"
        TmTAbs fi tyX knK1 t2 -> do
                ctx' <- addBinding fi tyX (TyVarBind knK1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll dummyInfo tyX knK1 tyT2
        TmTApp fi t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ _ knK11 tyT12 | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                        TyAll fi1 _ knK11 tyT12 -> throwError fi1 "Type argument has wrong kind"
                        _ -> throwError (getInfo tyT1) "universal type expected"
        TmLet fi x t1 t2 -> do
                tyT1 <- typeof ctx t1
                ctx' <- addBinding fi x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ typeShift (-1) tyT2
        TmFix fi t1 -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyArr fi1 tyT11 tyT12 -> do
                                tyeqv fi1 ctx tyT12 tyT11
                                return tyT12
                        _ -> throwError fi "arrow type expected"
        TmFold fi tyS -> case simplifyty ctx tyS of
                TyRec fi1 _ _ tyT -> return $ TyArr fi1 (typeSubstTop tyS tyT) tyS
                _ -> throwError fi "recursive type expected"
        TmUnfold fi tyS -> case simplifyty ctx tyS of
                TyRec fi1 _ _ tyT -> return $ TyArr fi1 tyS (typeSubstTop tyS tyT)
                _ -> throwError fi "recursive type expected"
        TmProj fi t1 l -> do
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyRecord _ fieldtys -> case lookup l fieldtys of
                                Just tyT -> return tyT
                                Nothing -> throwError fi $ "label " ++ show l ++ " not found"
                        _ -> throwError fi "Expected record type"
        TmRecord fi fields -> do
                fieldtys <- forM fields $ \(li, ti) -> do
                        tyTi <- typeof ctx ti
                        return (li, tyTi)
                return $ TyRecord dummyInfo fieldtys
        TmTag fi li ts1 tyT2 -> case simplifyty ctx tyT2 of
                TyVariant _ fieldtys -> case lookup li fieldtys of
                        Just tyTiExpected -> do
                                tyTi <- mapM (typeof ctx) ts1
                                mapM_ (uncurry $ tyeqv fi ctx) (zip tyTi tyTiExpected)
                                return tyT2
                        Nothing -> throwError fi $ "label " ++ pretty li ++ " not found"
                tyT2' -> throwError fi $ "Expected variant type, but got " ++ pretty (ctx, tyT2')
        TmCase fi t alts -> do
                tyT <- typeof ctx t
                case simplifyty ctx tyT of
                        TyVariant fi1 fieldtys -> do
                                when (null fieldtys) $ return ()
                                (tyT1 : restTy) <- forM alts $ \(li, (ki, ti)) -> case lookup li fieldtys of
                                        Just tys -> do
                                                ctx' <- (`execStateT` ctx) $
                                                        forM_ tys $ \tyT -> StateT $ \ctx -> do
                                                                let ctx' = addFreshName dummyVarName (VarBind tyT) ctx
                                                                return ((), ctx')
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing | nullName li -> do
                                                let ctx' = addFreshName dummyVarName (VarBind $ TyVariant fi1 fieldtys) ctx
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing -> throwError fi $ "label " ++ show li ++ " not found"
                                forM_ restTy $ \tyTi -> tyeqv fi ctx tyTi tyT1
                                return tyT1
                        tyT' -> throwError fi $ "Expected, but got " ++ pretty (ctx, tyT')

----------------------------------------------------------------
-- Type check of binding
----------------------------------------------------------------
checkBinding :: (MonadThrow m, MonadFail m) => Info -> Context -> Binding -> m ()
checkBinding fi ctx (TyAbbBind tyT (Just knK)) = do
        knK' <- kindof ctx tyT
        unless (knK == knK') $ throwError fi "Kind of binding does not match declared kind"
checkBinding fi ctx (TmAbbBind t (Just tyT)) = do
        tyT' <- typeof ctx t
        tyeqv fi ctx tyT tyT'
checkBinding _ _ _ = return ()
