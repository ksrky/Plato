module Plato.PsToTyp.Utils (HasDomain (..)) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing

class HasDomain a where
        getDomain :: a -> [Ident]

instance HasDomain a => HasDomain [a] where
        getDomain = concatMap getDomain

instance HasDomain a => HasDomain (Located a) where
        getDomain = getDomain . unLoc

instance HasDomain Pat where
        getDomain (ConP _ pats) = getDomain pats
        getDomain (VarP id) = [id]
        getDomain WildP = []
        getDomain (BinP lhs _ rhs) = getDomain lhs ++ getDomain rhs
        getDomain (AnnP pat _) = getDomain pat
        getDomain (FactorP pat) = getDomain pat

instance HasDomain TopDecl where
        getDomain (DataD id _ _) = [id]
        getDomain (LocalD ld) = getDomain ld

instance HasDomain LocDecl where
        getDomain (FunSpecD id _) = [id]
        getDomain FunBindD{} = []
        getDomain FixityD{} = []
