module Plato.Core.Scope where

import Data.Maybe
import GHC.Stack

import Plato.Common.Ident
import Plato.Core.Data
import Plato.Syntax.Core

emptyScope :: Scope
emptyScope = Scope []

extendScope :: Ident -> (Index, Maybe (Clos Type)) -> Scope -> Scope
extendScope id (i, a) (Scope sc) = Scope $ (id, (i, a)) : sc

lookupScope :: Ident -> Scope -> Maybe Index
lookupScope id (Scope sc) = do
        idCon <- lookup id sc
        return $! fst idCon

lookupCon :: HasCallStack => Scope -> Ident -> Maybe (Clos Type)
lookupCon (Scope s) x = do
        idCon <- lookup x s
        return $! fromJust $! snd idCon

class Closure a where
        getScope :: a -> Scope
        putScope :: a -> Scope -> a

instance Closure (Clos a) where
        getScope (_, s) = s
        putScope (a, _) s = (a, s)

instance Closure a => Closure (Bind a) where
        getScope (_, a) = getScope a
        putScope (x, a) s = (x, putScope a s)

instance Closure Boxed where
        getScope (Boxed c) = getScope c
        putScope (Boxed c) = Boxed . putScope c