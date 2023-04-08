module Plato.Syntax.Typing.Module where

import Prettyprinter

data Sig

data Decl

data Mod

data Bind

instance Eq Sig
instance Eq Decl
instance Eq Mod
instance Eq Bind

instance Show Sig
instance Show Decl
instance Show Mod
instance Show Bind

instance Pretty Sig
instance Pretty Decl
instance Pretty Mod
instance Pretty Bind
