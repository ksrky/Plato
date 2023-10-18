module Plato.Common.Path where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty

data Path
        = PIdent Ident
        | Path :.: Ident
        deriving (Eq, Show, Ord)

instance HasLoc Path where
        getLoc (PIdent id) = spanIdent id
        getLoc (root :.: field) = getLoc root <> getLoc field

instance Pretty Path where
        pretty (PIdent id) = pretty id
        pretty (root :.: field) = hcat [pretty root, dot, pretty field]

liftScope :: Path -> Ident -> Path
liftScope PIdent{} field = PIdent field
liftScope (root :.: _) field = root :.: field