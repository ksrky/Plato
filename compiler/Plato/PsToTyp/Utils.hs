{-# LANGUAGE GADTs #-}

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
        getDomain (L _ (BinP lhs _ rhs)) = getDomain lhs ++ getDomain rhs
        getDomain (L _ (FactorP pat)) = getDomain pat

instance HasDomain LDecl where
        getDomain (L _ (DataD id _ _)) = [id]
        getDomain (L _ (FunSpecD id _)) = [id]
        getDomain (L _ FunBindD{}) = []
        getDomain (L _ FixityD{}) = []