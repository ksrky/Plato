module Plato.Debug.EvaluateIO where

import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Evaluate
import Plato.Core.Syntax
import Plato.Debug.PrettyCore

evalIO :: Context -> Term -> IO Term
evalIO ctx t = case eval1 t of
        Just t' -> do
                putStrLn $ prcore ctx t ++ "\n"
                evalIO ctx t'
        Nothing -> return t
    where
        eval1 :: Term -> Maybe Term
        eval1 t = case t of
                TmApp (TmAbs (x, _) t12) v2 | isval v2 -> do
                        return $ termSubstTop v2 t12
                TmApp v1 t2 | isval v1 -> do
                        t2' <- eval1 t2
                        return $ TmApp v1 t2'
                TmApp t1 t2 -> do
                        t1' <- eval1 t1
                        return $ TmApp t1' t2
                TmTApp (TmTAbs (x, _) t11) tyT2 -> return $ tytermSubstTop tyT2 t11
                TmTApp t1 tyT2 -> do
                        t1' <- eval1 t1
                        return $ TmTApp t1' tyT2
                TmVar i n -> case getbinding ctx i n of
                        TmAbbBind t _ -> Just t
                        _ -> Nothing
                TmLet (x, v1) t2 | isval v1 -> Just $ termSubstTop v1 t2
                TmLet (x, t1) t2 -> do
                        t1' <- eval1 t1
                        Just $ TmLet (x, t1') t2
                TmTag l vs tyT | all isval vs -> Nothing
                TmTag l ts tyT -> do
                        ts' <- mapM eval1 ts
                        Just $ TmTag l ts' tyT
                (TmCase p@(TmTag li vs11 _) alts) | all isval vs11 -> case lookup li alts of
                        Just (_, body) -> do
                                return $ foldr termSubstTop body vs11
                        Nothing -> Nothing
                TmCase t1 alts -> do
                        t1' <- eval1 t1
                        Just $ TmCase t1' alts
                _ -> Nothing