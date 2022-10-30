module Plato.Typing.TcTypes where

import Prettyprinter (Pretty)

data TyVar

data MetaTv

instance Eq TyVar

instance Show TyVar

instance Eq MetaTv

instance Show MetaTv

instance Pretty TyVar