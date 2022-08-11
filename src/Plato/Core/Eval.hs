module Plato.Core.Eval where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Core.Utils

import Control.Exception.Safe
import Control.Monad.State

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Term -> Bool
isval t = case t of
        TmString{} -> True
        TmFloat{} -> True
        TmAbs{} -> True
        TmTag _ _ ts1 _ -> all isval ts1
        TmTAbs{} -> True
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar _ i n -> case getbinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp _ (TmAbs _ x _ t12) v2 | isval v2 -> do
                        return $ termSubstTop v2 t12
                TmApp fi v1@(TmTAbs _ x1 _ t12) v2 | isval v2 -> do
                        -- tmp: should be removed later
                        tyT2 <- typeof ctx v2
                        return $ TmApp fi (TmTApp fi v1 tyT2) v2
                TmApp fi v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp fi v1 t2'
                TmApp fi t1 t2 -> do
                        t1' <- eval1 t1
                        return $ TmApp fi t1' t2
                TmTApp _ (TmTAbs _ x _ t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp fi t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp fi t1' tyT2
                TmLet _ x v1 t2 | isval v1 -> Just $ termSubstTop v1 t2
                TmLet fi x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet fi x t1' t2
                TmTag _ l vs tyT | all isval vs -> Nothing
                TmTag fi l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag fi l ts' tyT
                TmCase _ (TmTag _ li vs11 _) alts | all isval vs11 -> case lookup li alts of
                        Just (_, body) -> do
                                return $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase fi t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase fi t1' alts
                _ -> Nothing

----------------------------------------------------------------
-- Type check
----------------------------------------------------------------
istyabb :: Context -> Int -> Bool
istyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT _ -> True
        _ -> False

gettyabb :: Context -> Int -> Ty
gettyabb ctx i = case getbinding ctx i of
        TyAbbBind tyT _ -> tyT
        _ -> error ""

computety :: Context -> Ty -> Maybe Ty
computety ctx tyT = case tyT of
        TyVar _ i _ | istyabb ctx i -> Just $ gettyabb ctx i
        _ -> Nothing

simplifyty :: Context -> Ty -> Ty
simplifyty ctx tyT = case computety ctx tyT of
        Just tyT' -> simplifyty ctx tyT'
        Nothing -> tyT

tyeqv :: MonadThrow m => Info -> Context -> Ty -> Ty -> m ()
tyeqv fi ctx = tyeqv'
    where
        tyeqv' :: MonadThrow m => Ty -> Ty -> m ()
        tyeqv' tyS tyT = do
                let tyS' = simplifyty ctx tyS
                    tyT' = simplifyty ctx tyT
                case (tyS', tyT') of
                        (TyVar _ i _, _) | istyabb ctx i -> tyeqv' (gettyabb ctx i) tyT
                        (_, TyVar _ i _) | istyabb ctx i -> tyeqv' tyS (gettyabb ctx i)
                        (TyVar _ i _, TyVar _ j _) | i == j -> return ()
                        (TyArr _ tyS1 tyS2, TyArr _ tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyAll _ tyX1 _ tyS2, TyAll _ _ _ tyT2) -> do
                                ctx' <- addname fi tyX1 ctx
                                tyeqv fi ctx' tyS2 tyT2
                        (TyAbs _ tyX1 knKS1 tyS2, TyAbs _ _ knKT1 tyT2) | knKS1 == knKT1 -> do
                                ctx' <- addname fi tyX1 ctx
                                tyeqv fi ctx' tyS2 tyT2
                        (TyApp _ tyS1 tyS2, TyApp _ tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyVariant fields1, TyVariant fields2) -> unreachable "TyVariant is unique"
                        _ -> throwError fi $ "type mismatch: " ++ pretty ctx tyS ++ ", " ++ pretty ctx tyT

----------------------------------------------------------------
-- kindof, typeof
----------------------------------------------------------------
getkind :: MonadThrow m => Info -> Context -> Int -> m Kind
getkind fi ctx i = case getbinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ (Just knK) -> return knK
        TyAbbBind _ Nothing -> throwError fi $ "No kind recorded for variable " ++ name2str (index2name ctx i)
        _ -> throwError fi $ "getkind: Wrong kind of binding for variable " ++ name2str (index2name ctx i)

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyArr _ tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                unless (knK1 == KnStar) $ throwError (getInfo tyT1) "star kind expected"
                knK2 <- kindof ctx tyT2
                unless (knK2 == KnStar) $ throwError (getInfo tyT2) "star kind expected"
                return KnStar
        TyVar fi i _ -> getkind fi ctx i
        TyAbs fi tyX knK1 tyT2 -> do
                ctx' <- addbinding fi tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp fi tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr knK11 knK12 -> throwError (getInfo tyT1) "parameter kind mismatch"
                        _ -> throwError fi "arrow kind expected"
        TyAll fi tyX knK1 tyT2 -> do
                ctx' <- addbinding fi tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                unless (knK2 == KnStar) $ throwError (getInfo tyT2) "Kind * expected"
                return KnStar
        TyVariant fields -> undefined

typeof :: (MonadThrow m, MonadFail m) => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar fi i _ -> getTypeFromContext fi ctx i
        TmAbs fi x tyT1 t2 -> do
                ctx' <- addbinding fi x (VarBind tyT1) ctx
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
                ctx' <- addbinding fi tyX (TyVarBind knK1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll dummyInfo tyX knK1 tyT2
        TmTApp fi t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ _ knK11 tyT12 | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                        TyAll fi1 _ knK11 tyT12 -> throwError fi1 "Type argument has wrong kind"
                        _ -> throwError (getInfo tyT1) "universal type expected"
        TmString{} -> undefined
        TmFloat{} -> undefined
        TmLet fi x t1 t2 -> do
                tyT1 <- typeof ctx t1
                ctx' <- addbinding fi x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ typeShift (-1) tyT2
        TmTag fi li ts tyT -> case simplifyty ctx tyT of
                TyVariant fieldtys -> case lookup li fieldtys of
                        Just tyTiExpected -> do
                                tyTi <- mapM (typeof ctx) ts
                                mapM_ (uncurry $ tyeqv fi ctx) (zip tyTi tyTiExpected)
                                return tyT
                        Nothing -> throwError fi $ "label " ++ name2str li ++ " not found"
                _ -> throwError fi "Expected variant type"
        TmCase fi t alts -> do
                t' <- typeof ctx t
                case simplifyty ctx t' of
                        TyVariant fieldtys -> do
                                when (null fieldtys) $ return ()
                                (tyT1 : restTy) <- forM alts $ \(li, (ki, ti)) -> case lookup li fieldtys of
                                        Just tys -> do
                                                ctx' <- (`execStateT` ctx) $
                                                        forM_ tys $ \tyT -> StateT $ \ctx -> do
                                                                ctx' <- addbinding dummyInfo dummyVarName (VarBind tyT) ctx
                                                                return ((), ctx')
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing | nullName li -> do
                                                ctx' <- addbinding dummyInfo dummyVarName (VarBind $ TyVariant fieldtys) ctx
                                                tyTi <- typeof ctx' ti
                                                return $ typeShift (- ki) tyTi
                                        Nothing -> throwError fi $ "label " ++ name2str li ++ " not found"
                                forM_ restTy $ \tyTi -> tyeqv fi ctx tyTi tyT1
                                return tyT1
                        _ -> throwError fi "Expected variant type"
