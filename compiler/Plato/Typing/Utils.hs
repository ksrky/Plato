module Plato.Typing.Utils where

import Plato.Common.Location
import Plato.Syntax.Typing

splitConstrTy :: Rho -> ([Sigma], Tau)
splitConstrTy = go []
    where
        go :: [Sigma] -> Rho -> ([Sigma], Tau)
        go acc (ArrT sigma rho) = go (unLoc sigma : acc) (unLoc rho)
        go acc tau = (reverse acc, tau)