module Plato.Common.Ident where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Map.Strict            qualified as M
import Prelude                    hiding (span)

import Plato.Common.Error
import Plato.Common.Fixity
import Plato.Common.Location
import Plato.Common.Name
import Plato.Common.Pretty
import Plato.Common.Uniq

-- | Identifier
data Ident = Ident
    { nameIdent   :: Name
    , spanIdent   :: Span
    , fixityIdent :: Fixity
    , stamp       :: Uniq
    }

instance Show Ident where
    show (Ident name _ _ uniq) = show name ++ "_" ++ show uniq

instance Eq Ident where
    id1 == id2 = stamp id1 == stamp id2

instance Ord Ident where
    compare id1 id2 = compare (stamp id1) (stamp id2)

instance HasLoc Ident where
    getLoc = spanIdent

instance Pretty Ident where
    pretty id = pretty (nameIdent id)

prettyId :: Ident -> Doc ann
prettyId id = pretty (nameIdent id) <> "_" <> pretty (stamp id)

ident :: Located Name -> Uniq -> Ident
ident (L sp name) uniq =
    Ident{nameIdent = name, spanIdent = sp, fixityIdent = identFixity, stamp = uniq}

oper :: Located Name -> Uniq -> Ident
oper (L sp name) uniq =
    Ident{nameIdent = name, spanIdent = sp, fixityIdent = operFixity, stamp = uniq}

fromIdent :: Ident -> Located Name
fromIdent id = L (getLoc id) (nameIdent id)

freshIdent :: (MonadReader e m, HasUniq e, MonadIO m) => Name -> m Ident
freshIdent name = do
    uniq <- pickUniq =<< ask
    return $ ident (noLoc name) uniq

setFixity :: Ident -> Fixity -> Ident
setFixity id fix = id{fixityIdent = fix}

reassignUniq :: (MonadReader e m, HasUniq e, MonadIO m) => Ident -> m Ident
reassignUniq id = do
    uniq <- pickUniq =<< ask
    return id{stamp = uniq}

-- | Identifier Map
type IdentMap a = M.Map Ident a

lookupIdent :: (MonadThrow m) => Ident -> M.Map Ident a -> m a
lookupIdent id idmap =
    case M.lookup id idmap of
        Just val -> return val
        Nothing  -> throwLocErr (spanIdent id) $ hsep ["Unknown identifier", squotes $ pretty id]
