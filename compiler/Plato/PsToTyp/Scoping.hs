module Plato.PsToTyp.Scoping (
        Scope,
        HasScope (..),
        scoping,
        extendScope,
) where

import Control.Exception.Safe
import Control.Monad.Reader.Class
import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name
import Plato.PsToTyp.Utils

type Scope = M.Map Name Ident

class HasScope e where
        getScope :: e -> Scope
        modifyScope :: (Scope -> Scope) -> e -> e
        setScope :: Scope -> e -> e
        setScope = modifyScope . const

instance HasScope Scope where
        getScope = id
        modifyScope = id

scoping :: (MonadReader e m, HasScope e, MonadThrow m) => Ident -> m Ident
scoping id = do
        sc <- asks getScope
        case M.lookup (nameIdent id) sc of
                Just id' -> return id{stamp = stamp id'}
                Nothing -> throwLocErr (getLoc id) $ hsep ["Not in scope", squotes $ pretty id]

extendScope :: (HasDomain a, HasScope e) => a -> e -> e
extendScope x = modifyScope (\sc -> foldr (\x -> M.insert (nameIdent x) x) sc (getDomain x))