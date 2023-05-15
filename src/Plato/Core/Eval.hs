module Plato.Core.Eval where

import Control.Monad (forM)

import Plato.Core.Calc
import Plato.Core.Env
import Plato.Syntax.Core

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: CoreEnv -> Term -> Bool
isval env t = case t of
        TmVar i _ -> case getBinding i env of
                TmVarBind{} -> True
                _ -> False
        TmAbs{} -> True
        TmTAbs{} -> True
        TmRecord fields -> all (\(_, vi) -> isval env vi) fields
        TmCon _ vs -> all (isval env) vs
        _ -> False

eval :: CoreEnv -> Term -> Term
eval env t = maybe t (eval env) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmVar i _ -> case getBinding i env of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmAbs _ _ t12) v2 | isval env v2 -> return $ substTop v2 t12
                TmApp v1 t2 | isval env v1 -> do
                        t2' <- eval' t2
                        return $ TmApp v1 t2'
                TmApp v1 t2 | isval env v1 -> do
                        t2' <- eval' t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmApp t1' t2
                TmTApp (TmTAbs _ _ t11) tyT2 -> Just $ substTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval' t1
                        Just $ TmTApp t1' tyT2
                TmLet _ v1 t2 | isval env v1 -> Just $ substTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval' t1
                        Just $ TmLet x t1' t2
                TmFix (TmAbs _ _ t12) -> Just $ substTop t t12
                TmFix t1 -> do
                        t1' <- eval' t1
                        Just $ TmFix t1'
                TmProj (TmRecord fields) l -> lookup l fields
                TmProj t1 l -> do
                        t1' <- eval' t1
                        Just $ TmProj t1' l
                TmRecord fields -> do
                        fields' <- forM fields $ \field -> case field of
                                (li, vi) | isval env vi -> Just (li, vi)
                                (li, ti) -> do
                                        ti' <- eval' ti
                                        Just (li, ti')
                        Just $ TmRecord fields'
                TmCon _ vs | all (isval env) vs -> Nothing
                TmCon l ts -> do
                        ts' <- mapM eval' ts
                        Just $ TmCon l ts'
                _ -> Nothing