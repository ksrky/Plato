module Plato.Common.Pretty where

class Pretty a where
        pretty :: a -> String
