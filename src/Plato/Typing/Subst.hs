module Plato.Typing.Subst where

import Control.Monad.Reader
import Data.Map.Strict qualified as M

import Plato.Common.Ident

type Subst a = M.Map Ident a

class HasSubst a where
        getSubst :: a -> Subst b

class Substitutable a where
        subst :: (MonadReader env m, HasSubst env) => a -> m a