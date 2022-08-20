module Plato.Abstract.Fixity where

import Plato.Abstract.Syntax

data Fixity
        = Infix Int
        | InfixL Int
        | InfixR Int
        deriving (Eq, Show)

getPrec :: Fixity -> Int
getPrec (Infix p) = p
getPrec (InfixL p) = p
getPrec (InfixR p) = p
