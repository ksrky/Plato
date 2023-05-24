module Plato.Scoping.Utils (allIdentsFromPats) where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Parsing

allIdentsFromPats :: [LPat] -> [Ident]
allIdentsFromPats = concatMap allIdentsFromPat

allIdentsFromPat :: LPat -> [Ident]
allIdentsFromPat (L _ (ConP _ pats)) = allIdentsFromPats pats
allIdentsFromPat (L _ (VarP id)) = [id]
allIdentsFromPat (L _ WildP) = []