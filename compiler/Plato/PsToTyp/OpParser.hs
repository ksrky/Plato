{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Plato.PsToTyp.OpParser where
{-
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Writer
import Data.Map.Strict qualified as M
import Data.Maybe

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.Parsing.OpParser.Resolver
import Plato.Parsing.Token
import Plato.Syntax.Parsing
import Data.Functor.Foldable

type FixityEnv = M.Map Name Fixity

class HasFixityEnv a where
        getFixityEnv :: a -> FixityEnv
        modifyFixityEnv :: (FixityEnv -> FixityEnv) -> a -> a
        setFixityEnv :: FixityEnv -> a -> a
        setFixityEnv = modifyFixityEnv . const

instance HasFixityEnv FixityEnv where
        getFixityEnv = id
        modifyFixityEnv = id

extendFixities :: HasFixityEnv e => [(Name, Fixity)] -> e -> e
extendFixities fixities = modifyFixityEnv $ \env -> foldr (uncurry M.insert) env fixities

class InfixExpansion a where
        expand :: (MonadReader e m, HasFixityEnv e, MonadThrow m) => Located a -> m [Tok b]

instance InfixExpansion Expr where
        expand (L _ (BinE lhs op rhs)) = do
                lhs' <- expand lhs
                rhs' <- expand rhs
                env <- asks getFixityEnv
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        expand exp = do
                exp' <- elabExpr exp
                return [TTerm exp']

instance InfixExpansion Pat where
        expand (L _ (BinP lhs op rhs)) = do
                lhs' <- expand lhs
                rhs' <- expand rhs
                env <- asks getFixityEnv
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op) env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        expand pat = do
                pat' <- elabPat pat
                return [TTerm pat']

instance InfixExpansion Type where
        expand (L _ (BinT lhs op rhs)) = do
                lhs' <- expand lhs
                rhs' <- expand rhs
                env <- asks getFixityEnv
                -- TODO: infix declaration does not distinguish ConOp or TyConOp
                let fix = fromMaybe defaultFixity (M.lookup (nameIdent op){nameSpace = ConName} env)
                return $ lhs' ++ [TOp op fix] ++ rhs'
        expand ty = do
                ty' <- elabType ty
                return [TTerm ty']
-}