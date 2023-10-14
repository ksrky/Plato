{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Plato.Syntax.Typing.Decl where

import Prettyprinter

import Data.Graph
import Plato.Syntax.Typing.Base

type role Bind nominal
data Bind (a :: TcFlag)

instance Eq (Bind a)
instance Show (Bind a)
instance Pretty (Bind a)

instance Pretty (SCC (Bind a))