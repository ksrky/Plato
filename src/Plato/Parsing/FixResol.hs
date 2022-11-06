{-# LANGUAGE OverloadedStrings #-}

module Plato.Parsing.FixResol where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Parsing.Fixity
import Plato.Syntax.Parsing

import Control.Exception.Safe
import Control.Monad
import qualified Data.Map.Strict as M

data Tok = TExp (Located Expr) | TOp Op deriving (Eq, Show)

linear :: MonadThrow m => OpTable -> Located Expr -> m [Tok]
linear optab (L _ (OpE e1 lx@(L _ x) e2)) = do
        e1' <- linear optab e1
        e2' <- linear optab e2
        let op = case M.lookup x optab of
                Just op' -> op'
                Nothing -> Op lx maxPrec Leftfix
        return $ e1' ++ [TOp op] ++ e2'
linear optab e = do
        e' <- resolve optab e
        return [TExp e']

class Resolver a where
        resolve :: MonadThrow m => OpTable -> Located a -> m (Located a)

instance Resolver Expr where
        resolve od = resolve'
            where
                resolve' :: MonadThrow m => Located Expr -> m (Located Expr)
                resolve' (L sp exp) =
                        L sp <$> case exp of
                                VarE{} -> return exp
                                AppE e1 e2 -> do
                                        e1' <- resolve' e1
                                        e2' <- resolve' e2
                                        return $ AppE e1' e2'
                                e@OpE{} -> do
                                        toks <- linear od (L sp e)
                                        res <- parseNeg (Op (L NoSpan (varName "")) (-1) Nonfix) toks
                                        case res of
                                                (L _ rese, []) -> return rese
                                                _ -> throwLocErr sp "Error at parsing infix expression"
                                    where
                                        parseNeg :: MonadThrow m => Op -> [Tok] -> m (Located Expr, [Tok])
                                        parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
                                        parseNeg _ (TOp _ : _) = error "unreachable: sequence of operator"
                                        parseNeg _ [] = error "unreachable: empty toks"
                                        parse :: MonadThrow m => Op -> Located Expr -> [Tok] -> m (Located Expr, [Tok])
                                        parse _ e1 [] = return (e1, [])
                                        parse op1@(Op _ prec1 fix1) e1 ((TOp op2@(Op lx prec2 fix2)) : rest)
                                                | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = throwLocErr sp "Error at parsing infix expression"
                                                | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = return (e1, TOp op2 : rest)
                                                | otherwise = do
                                                        (r, rest') <- parseNeg op2 rest
                                                        let sp' = combineSpans (getSpan e1) (getSpan r)
                                                        parse op1 (L sp' $ OpE e1 lx r) rest'
                                        parse _ _ _ = error "unrechable: sequence of expression"
                                LamE xs e -> do
                                        e' <- resolve' e
                                        return $ LamE xs e'
                                LetE ds e -> do
                                        ds' <- mapM (resolve od) ds
                                        e' <- resolve' e
                                        return $ LetE ds' e'
                                CaseE e alts -> do
                                        e' <- resolve' e
                                        alts' <- forM alts $ \(pi, ei) -> do
                                                ei' <- resolve' ei
                                                return (pi, ei')
                                        return $ CaseE e' alts'
                                FactorE e -> do
                                        L _ e' <- resolve' e
                                        return e'

instance Resolver Decl where
        resolve od (L sp d) =
                L sp <$> case d of
                        FuncD f args expr -> FuncD f args <$> resolve od expr
                        _ -> return d

instance Resolver TopDecl where
        resolve od (L sp td) =
                L sp <$> case td of
                        Decl d -> Decl <$> resolve od d
                        Eval e -> Eval <$> resolve od e
                        _ -> return td

resolveFixity :: MonadThrow m => OpTable -> Program -> m Program
resolveFixity od prg = do
        tds <- mapM (resolve od) (topDecls prg)
        return prg{topDecls = tds}
