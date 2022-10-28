module Plato.Core.Eval where

import Plato.Common.GlbName
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Core.Subst
import Plato.Syntax.Core

----------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------
isval :: Context -> Term -> Bool
isval ctx t = case t of
        TmAbs{} -> True
        TmRecord fields -> all (\(l, ti) -> isval ctx ti) fields
        TmTag _ ts1 _ -> all (isval ctx) ts1
        TmApp (TmFold _) v -> isval ctx v
        TmTAbs{} -> True
        TmVar i _ -> case getBinding ctx i of
                VarBind{} -> True
                _ -> False
        _ -> False

eval :: Context -> Term -> Term
eval ctx t = maybe t (eval ctx) (eval1 t)
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmVar i n -> case getBinding ctx i of
                        TmAbbBind t _ -> Just $ unLoc t
                        _ -> Nothing
                TmApp (TmUnfold tyS) (TmApp (TmFold tyT) v) | isval ctx v -> Just v
                TmApp (TmFold tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp (TmFold tyS) t2'
                TmApp (TmUnfold tyS) t2 -> do
                        t2' <- eval1 t2
                        Just $ TmApp (TmUnfold tyS) t2'
                TmApp (TmAbs x _ t12) v2 | isval ctx v2 -> do
                        return $ termSubstTop v2 t12
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp v1 t2'
                TmApp v1 t2 | isval ctx v1 -> do
                        t2' <- eval1 t2
                        Just $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmApp t1' t2
                TmTApp (TmTAbs x t11) tyT2 -> Just $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval1 t1
                        Just $ TmTApp t1' tyT2
                TmLet x v1 t2 | isval ctx v1 -> Just $ termSubstTop v1 t2
                TmLet x t1 t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet x t1' t2
                TmFix v1 | isval ctx v1 -> case v1 of
                        TmAbs _ _ t12 -> Just $ termSubstTop t t12
                        _ -> Nothing
                TmFix t1 -> do
                        t1' <- eval1 t1
                        Just $ TmFix t1'
                TmProj (TmRecord fields) l -> lookup l fields
                TmProj t1 l -> do
                        t1' <- eval1 t1
                        Just $ TmProj t1' l
                TmRecord fields -> do
                        let evalafield :: [(GlbName, Term)] -> Maybe [(GlbName, Term)]
                            evalafield l = case l of
                                [] -> Nothing
                                (l, vi) : rest | isval ctx vi -> do
                                        rest' <- evalafield rest
                                        Just $ (l, vi) : rest'
                                (l, ti) : rest -> do
                                        ti' <- eval1 ti
                                        Just $ (l, ti') : rest
                        fields' <- evalafield fields
                        Just $ TmRecord fields'
                TmTag l vs tyT | all (isval ctx) vs -> Nothing
                TmTag l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag l ts' tyT
                TmCase (TmTag li vs11 _) alts | all (isval ctx) vs11 -> case lookup li alts of
                        Just (_, body) -> Just $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase t1' alts
                _ -> Nothing