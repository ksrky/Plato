module Plato.PsToTyp.Utils (
        HasDomain (..),
) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing

class HasDomain a where
        getDomain :: a -> [Ident]

instance HasDomain a => HasDomain [a] where
        getDomain = concatMap getDomain

instance HasDomain LPat where
        getDomain (L _ (ConP _ pats)) = getDomain pats
        getDomain (L _ (VarP id)) = [id]
        getDomain (L _ WildP) = []

instance HasDomain LFunDecl where
        getDomain (L _ (FunSpec id _)) = [id]
        getDomain (L _ FunBind{}) = []
        getDomain (L _ FixDecl{}) = []