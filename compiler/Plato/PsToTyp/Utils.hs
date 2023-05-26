module Plato.PsToTyp.Utils (
        allIdentsFromPats,
        HasDomain (..),
) where

import Plato.Common.Error
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing

allIdentsFromPats :: [LPat] -> [Ident]
allIdentsFromPats = concatMap allIdentsFromPat

allIdentsFromPat :: LPat -> [Ident]
allIdentsFromPat (L _ (ConP _ pats)) = allIdentsFromPats pats
allIdentsFromPat (L _ (VarP id)) = [id]
allIdentsFromPat (L _ WildP) = []

class HasDomain a where
        getDomain :: a -> Ident

instance HasDomain LFunDecl where
        getDomain (L _ (FunSpec id _)) = id
        getDomain (L _ (FunBind id _)) = id
        getDomain (L _ FixDecl{}) = unreachable "deleted by Nicifier"