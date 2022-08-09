module Plato.Debug.Evaluate where

import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Evaluate
import Plato.Core.Syntax
import Plato.Core.Utils

import Control.Monad.State

evalIO :: Context -> Term -> IO Term
evalIO ctx t = case eval1 t of
        Just t' -> do
                --print (vmap fst ctx)
                --putStrLn ""
                putStrLn $ pretty ctx t ++ "\n"
                evalIO ctx t'
        Nothing -> return t
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