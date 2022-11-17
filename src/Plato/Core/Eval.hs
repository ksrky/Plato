module Plato.Core.Eval where

import Plato.Core.Context
import Plato.Core.Subst
import Plato.Syntax.Core

import Control.Monad (forM)

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmVar i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        TmAbs{} -> True
        TmTAbs{} -> True
        TmRecord fields -> all (\(_, vi) -> isval ctx vi) fields
        TmTag _ vs _ -> all (isval ctx) vs
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmVar i _ -> case getBinding ctx i of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmUnfold _) (TmApp (TmFold _) v) | isval ctx v -> Just v
                TmApp (TmFold tyS) t2 -> do
                        t2' <- eval' t2
                        Just $ TmApp (TmFold tyS) t2'
                TmApp (TmUnfold tyS) t2 -> do
                        t2' <- eval' t2
                        Just $ TmApp (TmUnfold tyS) t2'
                TmApp (TmAbs _ _ t12) v2 | isval ctx v2 -> return $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval' t2
                        return $ TmApp v1 t2'
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                TmTApp (TmTAbs _ t11) tyT2 -> Just $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval' t1
                        Just $ TmTApp t1' tyT2
                TmLet _ v1 t2 | isval ctx v1 -> Just $ termSubstTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmLet x t1' t2
                TmFix (TmAbs _ _ t12) -> Just $ termSubstTop t t12
                TmFix t1 -> do
                        t1' <- eval' t1
                        Just $ TmFix t1'
                TmProj (TmRecord fields) l -> lookup l fields
                TmProj t1 l -> do
                        t1' <- eval' t1
                        Just $ TmProj t1' l
                TmRecord fields -> do
                        fields' <- forM fields $ \field -> case field of
                                (li, vi) | isval ctx vi -> Just (li, vi)
                                (li, ti) -> do
                                        ti' <- eval' ti
                                        Just (li, ti')
                        Just $ TmRecord fields'
                TmTag _ vs _ | all (isval ctx) vs -> Nothing
                TmTag l ts tyT -> do
                        ts' <- mapM eval' ts
                        Just $ TmTag l ts' tyT
                TmCase (TmTag li vs11 _) alts | all (isval ctx) vs11 -> case lookup li alts of
                        Just (_, body) -> Just $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase t1 alts -> do
                        t1' <- eval' t1
                        Just $ TmCase t1' alts
                _ -> Nothing