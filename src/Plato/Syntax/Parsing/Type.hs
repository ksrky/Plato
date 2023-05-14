module Plato.Syntax.Parsing.Type where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location

----------------------------------------------------------------
-- Data and type
----------------------------------------------------------------
type LType = Located Type

data Type
        = VarT Ident
        | ConT Ident
        | -- | AppT LType LType
          ArrT LType LType
        | AllT [Ident] LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
-- instance Substitutable Type

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Type