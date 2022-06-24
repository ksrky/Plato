module Plato.Backend.Untyped where

import qualified Plato.Common.Name as N

data UTerm
        = UTmVar Int Int
        | UTmString String
        | UTmFloat Integer
        | UTmAbs N.Name UTerm
        | UTmLet N.Name UTerm UTerm
        | UTmCase UTerm [(UTerm, UTerm)]
        | UTmApp UTerm UTerm
        deriving (Eq, Show)
