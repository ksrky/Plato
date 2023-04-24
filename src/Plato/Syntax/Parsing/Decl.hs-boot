module Plato.Syntax.Parsing.Decl where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Path

type LDecl = Located Decl

data Decl

instance Show Decl
instance Eq Decl
instance Substitutable Decl
instance Pretty Decl