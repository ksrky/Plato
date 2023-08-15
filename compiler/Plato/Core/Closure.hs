module Plato.Core.Closure where

import Data.Map.Strict qualified as M
import Data.Maybe
import GHC.Stack

import Plato.Common.Ident
import Plato.Common.Pretty
import Plato.Syntax.Core

type Index = Int

newtype CoreScope = CoreScope (IdentMap (Index, Maybe (Clos Type))) deriving (Eq, Show)

type Clos a = (a, CoreScope)

instance PrettyWithContext a => PrettyWithContext (Clos a) where
        pretty' c (t, _) = pretty' c t

emptyScope :: CoreScope
emptyScope = CoreScope M.empty

extendScope :: Ident -> (Index, Maybe (Clos Type)) -> CoreScope -> CoreScope
extendScope id (i, a) (CoreScope sc) = CoreScope $ M.insert id (i, a) sc

lookupScope :: Ident -> CoreScope -> Maybe Index
lookupScope id (CoreScope sc) = do
        idCon <- M.lookup id sc
        return $! fst idCon

lookupCon :: HasCallStack => CoreScope -> Ident -> Maybe (Clos Type)
lookupCon (CoreScope s) x = do
        idCon <- M.lookup x s
        return $! fromJust $! snd idCon

class Closure a where
        getScope :: a -> CoreScope
        putScope :: a -> CoreScope -> a

instance Closure CoreScope where
        getScope = id
        putScope _ sc = sc

instance Closure (Clos a) where
        getScope (_, s) = s
        putScope (a, _) s = (a, s)

instance Closure a => Closure (Bind a) where
        getScope (_, a) = getScope a
        putScope (x, a) s = (x, putScope a s)
