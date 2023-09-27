{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Data.Graph
import Plato.Syntax.Typing.Base

type role Bind nominal
data Bind (a :: TcFlag)

instance Eq (Bind 'Untyped)
instance Eq (Bind 'Typed)
instance Show (Bind 'Untyped)
instance Show (Bind 'Typed)
instance Pretty (Bind 'Untyped)
instance Pretty (Bind 'Typed)

instance Pretty (SCC (Bind 'Untyped))
instance Pretty (SCC (Bind 'Typed))