{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Plato.Syntax.Typing.Decl where

import Data.Kind
import Prettyprinter

import Data.Graph
import Plato.Syntax.Typing.Base

type role Bind nominal
data Bind (a :: TcFlag)

instance Eq (Bind a)
instance Show (Bind a)
instance Pretty (Bind a)

data Rec :: Type -> Type

instance Eq a => Eq (Rec a)
instance Show a => Show (Rec a)
instance Pretty (Rec (Bind a))

instance Pretty (SCC (Bind a))