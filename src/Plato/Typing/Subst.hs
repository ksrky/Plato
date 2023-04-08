module Plato.Typing.Subst where

import qualified Data.Map.Strict as M
import qualified Data.Maybe

import Plato.Syntax.Typing.Ident as Ident
import Plato.Syntax.Typing.Path as Path

type Subst = M.Map Ident Path

class Substitutable a where
        subst :: Subst -> a -> a

instance Substitutable Path where
        subst sub p@(Path.PIdent id) = Data.Maybe.fromMaybe p (M.lookup id sub)
        subst sub (Path.PDot root field) = Path.PDot (subst sub root) field
