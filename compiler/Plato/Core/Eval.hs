module Plato.Core.Eval (eval) where

import Control.Monad (forM)

import Plato.Common.Utils
import Plato.Core.Calc
import Plato.Core.Env
import Plato.Syntax.Core

isval :: CoreEnv -> Term -> Bool
isval env t = case t of
        TmVar i _ -> case getBinding i env of
                TmVarBind{} -> True
                _ -> False
        TmAbs{} -> True
        TmTAbs{} -> True
        TmRecord fields -> all (\(_, vi) -> isval env vi) fields
        TmInj _ _ vs -> all (isval env) vs
        _ -> False

eval :: CoreEnv -> Term -> Term
eval env t = maybe t (eval env) (eval' t)
    where
        eval' :: Term -> Maybe Term
        eval' t = case t of
                TmVar i _ -> case getBinding i env of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmApp (TmUnfold _) (TmApp (TmFold _) v22) | isval env v22 -> Just v22
                TmApp (TmFold tyS) t2 -> do
                        t2' <- eval' t2
                        Just $ TmApp (TmFold tyS) t2'
                TmApp (TmUnfold tyS) t2 -> do
                        t2' <- eval' t2
                        Just $ TmApp (TmUnfold tyS) t2'
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
                TmProj (TmRecord fields) i | i < length fields -> snd <$> safeAt fields i
                TmProj t1 i -> do
                        t1' <- eval' t1
                        Just $ TmProj t1' i
                TmRecord fields -> do
                        fields' <- forM fields $ \field -> case field of
                                (li, vi) | isval env vi -> Just (li, vi)
                                (li, ti) -> do
                                        ti' <- eval' ti
                                        Just (li, ti')
                        Just $ TmRecord fields'
                TmInj _ _ vs | all (isval env) vs -> Nothing
                TmInj i tyT elims -> do
                        elims' <- forM elims $ \ti -> case ti of
                                vi | isval env vi -> Just vi
                                _ -> eval' ti
                        Just $ TmInj i tyT elims'
                TmCase (TmInj i _ elims) alts | all (isval env) elims -> case safeAt alts i of
                        Just (_, body) -> Just $ foldl substTop body elims
                        Nothing -> Nothing
                TmCase t alts -> do
                        t' <- eval' t
                        Just $ TmCase t' alts
                _ -> Nothing