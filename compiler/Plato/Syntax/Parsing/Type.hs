module Plato.Syntax.Parsing.Type where

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty

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
    | BinT LType Ident LType
    | FactorT LType
    deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Type where
    pretty' _ (VarT var) = pretty var
    pretty' _ (ConT con) = pretty con
    pretty' p (AppT fun arg) = parenswPrec p 1 $ pretty' 1 fun <+> pretty' 2 arg
    pretty' p (ArrT arg res) = parenswPrec p 0 $ hsep [pretty' 1 arg, arrow, pretty res]
    pretty' p (AllT vars body) = parenswPrec p 0 $ hsep [braces (hsep (map pretty vars)), pretty body]
    pretty' _ (BinT lhs op rhs) = parens $ hsep [pretty' 1 lhs, pretty op, pretty' 1 rhs]
    pretty' _ (FactorT ty) = parens $ pretty ty
