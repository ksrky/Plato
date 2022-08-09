module Plato.Core.Evaluate where

import Plato.Common.Error
import Plato.Common.Info
import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Pretty
import Plato.Core.Syntax

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
                        tyT2 <- typeof ctx v2 `evalStateT` ctx
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
        TyVar i _ | istyabb ctx i -> Just $ gettyabb ctx i
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
                        (TyVar i _, _) | istyabb ctx i -> tyeqv' (gettyabb ctx i) tyT
                        (_, TyVar i _) | istyabb ctx i -> tyeqv' tyS (gettyabb ctx i)
                        (TyVar i _, TyVar j _) | i == j -> return ()
                        (TyArr tyS1 tyS2, TyArr tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyAll tyX1 _ tyS2, TyAll _ _ tyT2) -> do
                                let ctx' = addname tyX1 ctx
                                tyeqv fi ctx' tyS2 tyT2
                        (TyAbs tyX1 knKS1 tyS2, TyAbs _ knKT1 tyT2) | knKS1 == knKT1 -> do
                                let ctx' = addname tyX1 ctx
                                tyeqv fi ctx' tyS2 tyT2
                        (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> do
                                tyeqv' tyS1 tyT1
                                tyeqv' tyS2 tyT2
                        (TyVariant fields1, TyVariant fields2) -> unreachable "TyVariant is unique"
                        _ -> throwError fi $ "type mismatch: " ++ pretty ctx tyS ++ ", " ++ pretty ctx tyT

----------------------------------------------------------------
-- kindof, typeof
----------------------------------------------------------------
getkind :: MonadThrow m => Context -> Int -> m Kind
getkind ctx i = case getbinding ctx i of
        TyVarBind knK -> return knK
        TyAbbBind _ (Just knK) -> return knK
        TyAbbBind _ Nothing -> throwString $ "No kind recorded for variable " ++ name2str (index2name ctx i)
        _ -> throwString $ "getkind: Wrong kind of binding for variable " ++ name2str (index2name ctx i)

kindof :: MonadThrow m => Context -> Ty -> m Kind
kindof ctx tyT = case tyT of
        TyArr tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                unless (knK1 == KnStar) $ throwString "star kind expected"
                knK2 <- kindof ctx tyT2
                unless (knK2 == KnStar) $ throwString "star kind expected"
                return KnStar
        TyVar i _ -> getkind ctx i
        TyAbs tyX knK1 tyT2 -> do
                let ctx' = addbinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                return $ KnArr knK1 knK2
        TyApp tyT1 tyT2 -> do
                knK1 <- kindof ctx tyT1
                knK2 <- kindof ctx tyT2
                case knK1 of
                        KnArr knK11 knK12 | knK2 == knK11 -> return knK12
                        KnArr knK11 knK12 -> throwString "parameter kind mismatch"
                        _ -> throwString "arrow kind expected"
        TyAll tyX knK1 tyT2 -> do
                let ctx' = addbinding tyX (TyVarBind knK1) ctx
                knK2 <- kindof ctx' tyT2
                unless (knK2 == KnStar) $ throwString "Kind * expected"
                return KnStar
        TyVariant fields -> undefined

typeof :: MonadThrow m => Context -> Term -> m Ty
typeof ctx t = case t of
        TmVar fi i _ -> getTypeFromContext fi ctx i
        TmAbs _ x tyT1 t2 -> do
                let ctx' = addbinding x (VarBind tyT1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyArr tyT1 (typeShift (-1) tyT2)
        TmApp fi t1 t2 -> do
                tyT1 <- typeof ctx t1
                tyT2 <- typeof ctx t2
                case tyT1 of
                        TyArr tyT11 tyT12 -> do
                                tyeqv fi ctx tyT2 tyT11
                                return tyT12
                        _ -> throwError fi "arrow type expected"
        TmTAbs _ tyX knK1 t2 -> do
                let ctx' = addbinding tyX (TyVarBind knK1) ctx
                tyT2 <- typeof ctx' t2
                return $ TyAll tyX knK1 tyT2
        TmTApp fi t1 tyT2 -> do
                knKT2 <- kindof ctx tyT2
                tyT1 <- typeof ctx t1
                case simplifyty ctx tyT1 of
                        TyAll _ knK11 tyT12 | knK11 == knKT2 -> return $ typeSubstTop tyT2 tyT12
                        TyAll _ knK11 tyT12 -> throwString "Type argument has wrong kind"
                        _ -> throwString "universal type expected"
        TmString{} -> undefined
        TmFloat{} -> undefined
        TmLet fi x t1 t2 -> do
                tyT1 <- typeof ctx t1
                let ctx' = addbinding x (VarBind tyT1) ctx
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
                                forM_ alts $ \(li, (xi, ti)) -> case lookup li fieldtys of
                                        Just _ -> return ()
                                        Nothing -> throwError fi $ "label " ++ name2str li ++ " not in type"
                                casetypes <- forM alts $ \(li, (xi, ti)) -> case lookup li fieldtys of
                                        Just tys -> do
                                                let ctx' = mapM_ (modify . addbinding dummyName . VarBind) tys `execState` ctx
                                                ti' <- typeof ctx' ti
                                                return $ typeShift (-1) ti'
                                        Nothing -> throwError fi $ "label " ++ name2str li ++ " not found"
                                let (tyT1 : restTy) = casetypes
                                forM_ restTy $ \tyTi -> tyeqv fi ctx tyTi tyT1
                                return tyT1
                        _ -> throwError fi "Expected variant type"
