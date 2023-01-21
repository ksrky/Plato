module Plato.Parsing.FixResol (resolveFixity) where

import Plato.Syntax.Parsing

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Location

import Control.Exception.Safe
import Control.Monad
import qualified Data.Map.Strict as M

data Tok = TExp LExpr | TOp LPsName Fixity deriving (Eq, Show)

resolveFixity :: MonadThrow m => FixityEnv PsName -> LExpr -> m LExpr
resolveFixity fixenv exp = resolve exp
    where
        linear :: MonadThrow m => LExpr -> m [Tok]
        linear (L _ (OpE e1 x e2)) = do
                e1' <- linear e1
                e2' <- linear e2
                let fix = case M.lookup (unLoc x) fixenv of
                        Just op' -> op'
                        Nothing -> Fixity maxPrec Leftfix
                return $ e1' ++ [TOp x fix] ++ e2'
        linear e = do
                e' <- resolve e
                return [TExp e']

        resolve :: MonadThrow m => LExpr -> m LExpr
        resolve (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE e1 e2 -> do
                                e1' <- resolve e1
                                e2' <- resolve e2
                                return $ AppE e1' e2'
                        e@OpE{} -> do
                                toks <- linear (L sp e)
                                res <- parseNeg (Fixity (-1) Nonfix) toks
                                case res of
                                        (L _ rese, []) -> return rese
                                        _ -> throwLocErr sp "Error at parsing infix expression"
                            where
                                parseNeg :: MonadThrow m => Fixity -> [Tok] -> m (LExpr, [Tok])
                                parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
                                parseNeg _ (TOp _ _ : _) = error "unreachable: sequence of operator"
                                parseNeg _ [] = error "unreachable: empty toks"
                                parse :: MonadThrow m => Fixity -> LExpr -> [Tok] -> m (LExpr, [Tok])
                                parse _ e1 [] = return (e1, [])
                                parse op1@(Fixity prec1 fix1) e1 ((TOp name op2@(Fixity prec2 fix2)) : rest)
                                        | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = throwLocErr sp "Error at parsing infix expression"
                                        | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = return (e1, TOp name op2 : rest)
                                        | otherwise = do
                                                (r, rest') <- parseNeg op2 rest
                                                let sp' = combineSpans (getLoc e1) (getLoc r)
                                                parse op1 (L sp' $ OpE e1 name r) rest'
                                parse _ _ _ = error "unrechable: sequence of expression"
                        LamE xs e -> do
                                e' <- resolve e
                                return $ LamE xs e'
                        LetE ds e -> do
                                -- note: do not traverse let binds
                                e' <- resolve e
                                return $ LetE ds e'
                        CaseE e alts -> do
                                e' <- resolve e
                                alts' <- forM alts $ \(pi, ei) -> do
                                        ei' <- resolve ei
                                        return (pi, ei')
                                return $ CaseE e' alts'
                        FactorE e -> do
                                L _ e' <- resolve e
                                return e'