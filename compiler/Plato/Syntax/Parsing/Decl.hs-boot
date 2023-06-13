module Plato.Syntax.Parsing.Decl where

import Prettyprinter

import Plato.Common.Location

type LDecl = Located Decl

data Decl

instance Eq Decl
instance Show Decl
instance Pretty Decl