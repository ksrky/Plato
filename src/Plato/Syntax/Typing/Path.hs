module Plato.Syntax.Typing.Path where

import Prettyprinter

import Plato.Common.Location ( combineSpans, Span )
import Plato.Syntax.Typing.Ident as Ident

-- | Path
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