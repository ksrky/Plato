module Plato.Common.Ident where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import qualified Data.Map.Strict as M
import Prettyprinter
import Prelude hiding (span)

import Plato.Common.Error
import Plato.Common.Global as Global
import Plato.Common.Location
import Plato.Common.Name

-- | Identifier
data Ident = Ident {name :: Name, span :: Span, stamp :: Unique}
        deriving (Ord, Show)

instance Eq Ident where
        id1 == id2 = stamp id1 == stamp id2

instance Pretty Ident where
        pretty id = pretty (name id)

ident :: Located Name -> Int -> Ident
ident (L sp x) i = Ident{name = x, span = sp, stamp = i}

fresh :: (MonadReader glb m, HasUnique glb, MonadIO m) => Name -> m Ident
fresh x = do
        glb <- ask
        i <- Global.getUnique glb
        return Ident{name = x, span = NoSpan, stamp = i}

-- | Identifier Map
type IdentMap a = M.Map Ident a

lookup :: MonadThrow m => Ident -> M.Map Ident a -> m a
lookup id idmap = case M.lookup id idmap of
        Just val -> return val
        Nothing -> throwLocErr (span id) $ hsep ["Not in scope ", squotes $ pretty id]