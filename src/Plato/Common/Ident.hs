module Plato.Common.Ident where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import qualified Data.Map.Strict as M
import Prettyprinter
import Prelude hiding (span)

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Uniq

-- | Identifier
data Ident = Ident {nameIdent :: Name, spanIdent :: Span, stamp :: Uniq}
        deriving (Ord, Show)

instance Eq Ident where
        id1 == id2 = stamp id1 == stamp id2

instance GetLoc Ident where
        getLoc = spanIdent

instance Pretty Ident where
        pretty id = pretty (nameIdent id)

ident :: Located Name -> Uniq -> Ident
ident (L sp x) u = Ident{nameIdent = x, spanIdent = sp, stamp = u}

fromIdent :: Ident -> Located Name
fromIdent id = L (getLoc id) (nameIdent id)

freshIdent :: (MonadReader ctx m, HasUniq ctx, MonadIO m) => Name -> m Ident
freshIdent x = do
        u <- pickUniq =<< ask
        return Ident{nameIdent = x, spanIdent = NoSpan, stamp = u}

-- | Identifier Map
type IdentMap a = M.Map Ident a

lookupIdent :: MonadThrow m => Ident -> M.Map Ident a -> m a
lookupIdent id idmap = case M.lookup id idmap of
        Just val -> return val
        Nothing -> throwLocErr (spanIdent id) $ hsep ["Not in scope ", squotes $ pretty id]