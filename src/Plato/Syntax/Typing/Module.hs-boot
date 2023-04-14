module Plato.Syntax.Typing.Module where

import Plato.Common.Path (Substitutable)
import Prettyprinter

data Decl

instance Eq Decl
instance Show Decl
instance Substitutable Decl
instance Pretty Decl