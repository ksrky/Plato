{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Plato.Nicifier.OpParser (opParse) where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans.Writer
import Data.Map.Strict qualified as M

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Nicifier.OpParser.Fixity
import Plato.Nicifier.OpParser.Resolver
import Plato.Syntax.Parsing

linearize :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => LExpr -> m [Tok Expr]
linearize (L _ (InfixE lhs op rhs)) = do
        lhs' <- linearize lhs
        rhs' <- linearize rhs
        fix <-
                asks getFixityEnv
                        >>= ( \case
                                Just op' -> return op'
                                Nothing -> return defaultFixity
                            )
                                . M.lookup (nameIdent op)
        return $ lhs' ++ [TOp op fix] ++ rhs'
linearize exp = do
        exp' <- opParse exp
        return [TTerm exp']

-- | Operator parser
class OpParser a where
        opParse :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => a -> m a

instance OpParser LExpr where
        opParse (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE fun arg -> AppE <$> opParse fun <*> opParse arg
                        InfixE{} -> do
                                toks <- linearize (L sp exp)
                                unLoc <$> parse InfixE toks
                        LamE var exp -> LamE var <$> opParse exp
                        LetE decs body -> do
                                decs' <- opParse decs
                                body' <- opParse body
                                return $ LetE decs' body'
                        CaseE match alts -> do
                                match' <- opParse match
                                alts' <- mapM (\(p, e) -> (p,) <$> opParse e) alts
                                return $ CaseE match' alts'
                        FactorE e -> unLoc <$> opParse e

instance OpParser Clause where
        opParse (pats, exp) = (pats,) <$> opParse exp

instance OpParser [LDecl] where
        opParse decs = local (modifyFixityEnv extendFixity) $ forM rest $ \case
                L sp (FunBindD id clauses) -> L sp <$> (FunBindD id <$> mapM opParse clauses)
                dec -> return dec
            where
                (fixmap, rest) = execWriter $ forM decs $ \case
                        L _ (FixityD id fix) -> tell ([(nameIdent id, fix)], [])
                        d -> tell ([], [d])
                extendFixity :: FixityEnv -> FixityEnv
                extendFixity env = foldr (uncurry M.insert) env fixmap