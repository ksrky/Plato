{-# LANGUAGE FlexibleInstances #-}

module Plato.Common.Path where

import Prettyprinter

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import Control.Monad.Reader.Class
import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Name

------------------
--     Path     --
------------------
data Path
        = PIdent Ident
        | PDot Path (Located Name)
        deriving (Eq, Ord, Show)

instance GetLoc Path where
        getLoc (PIdent id) = spanIdent id
        getLoc (PDot root field) = combineSpans (getLoc root) (getLoc field)

instance Pretty Path where
        pretty (PIdent id) = pretty id
        pretty (PDot root field) = hcat [pretty root, dot, pretty field]

------------------
-- Substitution --
------------------
type Subst = M.Map Ident Path

class HasSubst a where
        getSubst :: a -> Subst

instance HasSubst Subst where
        getSubst = id

class Substitutable a where
        substPath :: (MonadReader ctx m, HasSubst ctx) => a -> m a

instance Substitutable Path where
        substPath p@(PIdent id) = do
                sub <- asks getSubst
                return $ fromMaybe p (M.lookup id sub)
        substPath (PDot root field) = (PDot <$> substPath root) <*> return field
