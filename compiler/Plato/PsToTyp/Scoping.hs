module Plato.PsToTyp.Scoping (
        Scope,
        HasScope (..),
        initScope,
        scoping,
        extendScopeFromSeq,
) where

import Control.Exception.Safe
import Control.Monad.Reader
import Data.Map.Strict qualified as M

import Plato.Common.Ident
import Plato.Common.Name
import Plato.PsToTyp.Error
import Plato.PsToTyp.Utils

type Scope = M.Map Name Ident

class HasScope a where
        getScope :: a -> Scope
        modifyScope :: (Scope -> Scope) -> a -> a
        extendScope :: Ident -> a -> a
        extendScope id = modifyScope (M.insert (nameIdent id) id)
        extendListScope :: [Ident] -> a -> a
        extendListScope ids env = foldr extendScope env ids

instance HasScope Scope where
        getScope = id
        modifyScope = id

initScope :: Scope
initScope = M.empty

scoping :: (MonadReader e m, HasScope e, MonadThrow m) => Ident -> m Ident
scoping id = do
        sc <- asks getScope
        case M.lookup (nameIdent id) sc of
                Just id' -> return id{stamp = stamp id'}
                Nothing -> throwScopeError id

extendScopeFromSeq :: (MonadReader env m, HasScope env, HasDomain a) => [a] -> m env
extendScopeFromSeq seq = asks (extendListScope (getDomain seq))