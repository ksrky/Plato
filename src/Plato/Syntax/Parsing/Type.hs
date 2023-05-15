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
        | ArrT LType LType
        | AllT [Ident] LType
        | AppT LType LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Type