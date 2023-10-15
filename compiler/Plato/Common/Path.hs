module Plato.Common.Path where

import Control.Exception.Safe
import Data.Map.Strict qualified as M

import Plato.Common.Error
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

type PathMap a = M.Map Path a

lookupPath :: (MonadThrow m) => Path -> PathMap a -> m a
lookupPath p pmap = case M.lookup p pmap of
        Just val -> return val
        Nothing -> throwLocErr (getLoc p) $ hsep ["Unknown identifier", squotes $ pretty p]