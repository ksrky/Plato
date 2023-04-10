module Plato.Common.Path where

import Prettyprinter

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Plato.Common.Ident as Ident
import Plato.Common.Location (Span, combineSpans)

------------------
--     Path     --
------------------
data Path
        = PIdent Ident
        | PDot Path Ident
        deriving (Eq, Ord, Show)

getLoc :: Path -> Span
getLoc (PIdent id) = Ident.span id
getLoc (PDot root field) = combineSpans (getLoc root) (Ident.span field)

instance Pretty Path where
        pretty (PIdent id) = pretty id
        pretty (PDot root field) = hcat [pretty root, dot, pretty field]

------------------
-- Substitution --
------------------
type Subst = M.Map Ident Path

class Substitutable a where
        subst :: Subst -> a -> a

instance Substitutable Path where
        subst sub p@(PIdent id) = fromMaybe p (M.lookup id sub)
        subst sub (PDot root field) = PDot (subst sub root) field
