module Plato.Core.Closure where

import Data.Map.Strict qualified as M
import Data.Maybe
import GHC.Stack

import Plato.Common.Ident
import Plato.Common.Pretty
import Plato.Syntax.Core

type Ix = Int

newtype Scope = Scope (IdentMap (Ix, Maybe (Clos Type))) deriving (Eq, Show)

type Clos a = (a, Scope)

instance PrettyWithContext a => PrettyWithContext (Clos a) where
        pretty' c (t, _) = pretty' c t

emptyScope :: Scope
emptyScope = Scope M.empty

extendScope :: Ident -> (Ix, Maybe (Clos Type)) -> Scope -> Scope
extendScope id (i, a) (Scope sc) = Scope $ M.insert id (i, a) sc

lookupScope :: Ident -> Scope -> Maybe Ix
lookupScope id (Scope sc) = do
        idCon <- M.lookup id sc
        return $! fst idCon

lookupCon :: HasCallStack => Scope -> Ident -> Maybe (Clos Type)
lookupCon (Scope s) x = do
        idCon <- M.lookup x s
        return $! fromJust $! snd idCon

class Closure a where
        getScope :: a -> Scope
        putScope :: a -> Scope -> a

instance Closure Scope where
        getScope = id
        putScope _ sc = sc

instance Closure (Clos a) where
        getScope (_, s) = s
        putScope (a, _) s = (a, s)

instance Closure a => Closure (Bind a) where
        getScope (_, a) = getScope a
        putScope (x, a) s = (x, putScope a s)
