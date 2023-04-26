{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Plato.PsToTyp.OpParser where

import Control.Exception.Safe
import Control.Monad.Reader.Class

import qualified Data.Map.Strict as M
import Plato.Common.Fixity
import Plato.Common.Ident
import Plato.Common.Location
import Plato.PsToTyp.OpParser.Resolver
import Plato.Syntax.Parsing

-- Operator parser
class OpParser a where
        opParser ::
                (MonadReader env m, HasFixityEnv env, MonadThrow m) =>
                Located a ->
                m (Located a)
        linearize :: (MonadReader env m, HasFixityEnv env, MonadThrow m) => Located a -> m [Tok a]

instance OpParser Expr where
        opParser (L sp exp) =
                L sp <$> case exp of
                        VarE{} -> return exp
                        AppE e1 e2 -> AppE <$> opParser e1 <*> opParser e2
                        OpE{} -> do
                                toks <- linearize (L sp exp)
                                unLoc <$> parse OpE toks
                        LamE alts -> do
                                LamE
                                        <$> mapM
                                                ( \(pats, body) -> (pats,) <$> opParser body
                                                )
                                                alts
                        LetE ds e -> do
                                -- note: do not traverse let binds
                                e' <- opParser e
                                return $ LetE ds e'
                        CaseE e alts ->
                                CaseE <$> opParser e
                                        <*> mapM
                                                ( \(pi, ei) -> (pi,) <$> opParser ei
                                                )
                                                alts
                        FactorE e -> unLoc <$> opParser e
        linearize (L _ (OpE lhs op rhs)) = do
                lhs' <- linearize lhs
                rhs' <- linearize rhs
                let id = undefined :: Ident
                fix <-
                        asks getFixityEnv
                                >>= ( \case
                                        Just op' -> return op'
                                        Nothing -> return $ Fixity maxPrec Leftfix
                                    )
                                        . M.lookup id
                return $ lhs' ++ [TOp op fix] ++ rhs'
        linearize exp = do
                exp' <- opParser exp
                return [TTerm exp']
