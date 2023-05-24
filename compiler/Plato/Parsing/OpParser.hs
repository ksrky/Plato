{-# LANGUAGE LambdaCase #-}

module Plato.Parsing.OpParser (opParser) where

import Control.Exception.Safe
import Control.Monad.Reader.Class
import Data.Map.Strict qualified as M
import Data.Maybe qualified as Maybe

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Parsing.OpParser.Fixity
import Plato.Parsing.OpParser.Resolver
import Plato.Syntax.Parsing

linearize :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => LExpr -> m [Tok Expr]
linearize (L _ (OpE lhs op rhs)) = do
        lhs' <- linearize lhs
        rhs' <- linearize rhs
        fix <-
                asks getFixityEnv
                        >>= ( \case
                                Just op' -> return op'
                                Nothing -> return $ Fixity maxPrec Leftfix
                            )
                                . M.lookup (nameIdent op)
        return $ lhs' ++ [TOp op fix] ++ rhs'
linearize exp = do
        exp' <- opParser exp
        return [TTerm exp']

-- | Operator parser
opParser :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => Located Expr -> m (Located Expr)
opParser (L sp exp) =
        L sp <$> case exp of
                VarE{} -> return exp
                AppE fun arg -> AppE <$> opParser fun <*> opParser arg
                OpE{} -> do
                        toks <- linearize (L sp exp)
                        unLoc <$> parse OpE toks
                LamE var exp -> LamE var <$> opParser exp
                LetE decs body -> do
                        decs' <- opParserFunDecls decs
                        body' <- opParser body
                        return $ LetE decs' body'
                FactorE e -> unLoc <$> opParser e

opParserClause :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => Clause -> m Clause
opParserClause (pats, exp) = (pats,) <$> opParser exp

opParserFunDecls :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => [LFunDecl] -> m [LFunDecl]
opParserFunDecls decs = do
        let defs = [id | L _ (FunSpec id _) <- decs]
            extends :: FixityEnv -> FixityEnv
            extends env = foldr (\id -> M.insert (nameIdent id) defaultFixity) env defs
            addFixity :: Ident -> Fixity -> FixityEnv -> FixityEnv
            addFixity id fix =
                M.alter
                        (Maybe.maybe (throwLocErr (getLoc id) "Operator not in scope") (const $ Just fix))
                        (nameIdent id)
            fixds = [(id, fix) | L _ (FixDecl id fix) <- decs]
            extends' env = foldr (uncurry addFixity) (extends env) fixds
        local (modifyFixityEnv extends') $ mapM opParserFunD decs
    where
        opParserFunD :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => LFunDecl -> m LFunDecl
        opParserFunD (L sp dec) = case dec of
                FunBind id clauses -> L sp <$> (FunBind id <$> mapM opParserClause clauses)
                _ -> return $ L sp dec