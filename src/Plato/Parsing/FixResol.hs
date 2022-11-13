module Plato.Parsing.FixResol where

import Plato.Syntax.Parsing

import Plato.Types.Error
import Plato.Types.Fixity
import Plato.Types.Location
import Plato.Types.Name.Global

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map.Strict as M

data Tok = TExp (LExpr GlbName) | TOp GlbName Fixity deriving (Eq, Show)

linear :: MonadThrow m => LExpr GlbName -> ReaderT (FixityEnv GlbName) m [Tok]
linear (L _ (OpE e1 (L _ x) e2)) = do
        e1' <- linear e1
        e2' <- linear e2
        fixenv <- ask
        let fix = case M.lookup x fixenv of
                Just op' -> op'
                Nothing -> Fixity maxPrec Leftfix
        return $ e1' ++ [TOp x fix] ++ e2'
linear e = do
        e' <- resolve e
        return [TExp e']

class Resolver f where
        resolve :: MonadThrow m => Located (f GlbName) -> ReaderT (FixityEnv GlbName) m (Located (f GlbName))

instance Resolver Expr where
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
                                parseNeg :: MonadThrow m => Fixity -> [Tok] -> m (LExpr GlbName, [Tok])
                                parseNeg op1 (TExp e1 : rest) = parse op1 e1 rest
                                parseNeg _ (TOp _ _ : _) = error "unreachable: sequence of operator"
                                parseNeg _ [] = error "unreachable: empty toks"
                                parse :: MonadThrow m => Fixity -> LExpr GlbName -> [Tok] -> m (LExpr GlbName, [Tok])
                                parse _ e1 [] = return (e1, [])
                                parse op1@(Fixity prec1 fix1) e1 ((TOp glbn op2@(Fixity prec2 fix2)) : rest)
                                        | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix) = throwLocErr sp "Error at parsing infix expression"
                                        | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix) = return (e1, TOp glbn op2 : rest)
                                        | otherwise = do
                                                (r, rest') <- parseNeg op2 rest
                                                let sp' = combineSpans (getLoc e1) (getLoc r)
                                                parse op1 (L sp' $ OpE e1 (L (g_loc glbn) glbn) r) rest'
                                parse _ _ _ = error "unrechable: sequence of expression"
                        LamE xs e -> do
                                e' <- resolve e
                                return $ LamE xs e'
                        LetE ds e -> do
                                ds' <- mapM resolve ds
                                e' <- resolve e
                                return $ LetE ds' e'
                        CaseE e alts -> do
                                e' <- resolve e
                                alts' <- forM alts $ \(pi, ei) -> do
                                        ei' <- resolve ei
                                        return (pi, ei')
                                return $ CaseE e' alts'
                        FactorE e -> do
                                L _ e' <- resolve e
                                return e'

instance Resolver Decl where
        resolve (L sp d) =
                L sp <$> case d of
                        FuncD f args expr -> FuncD f args <$> resolve expr
                        _ -> return d

instance Resolver TopDecl where
        resolve (L sp td) =
                L sp <$> case td of
                        Decl d -> Decl <$> resolve d
                        Eval e -> Eval <$> resolve e
                        _ -> return td

resolveFixity :: MonadThrow m => FixityEnv GlbName -> Program GlbName -> m (Program GlbName)
resolveFixity fixenv prg = do
        tds <- runReaderT (mapM resolve (ps_topDecls prg)) fixenv
        return prg{ps_topDecls = tds}