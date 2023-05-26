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
instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> prty AppPrec (unLoc arg)
        pretty (ArrT arg res) = prty ArrPrec (unLoc arg) <+> "->" <+> prty TopPrec (unLoc res)
        pretty (AllT vars body) = braces (hsep (map pretty vars)) <+> pretty body

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precOf :: Type -> Prec
precOf AllT{} = TopPrec
precOf ArrT{} = ArrPrec
precOf _ = AtomPrec

prty :: Prec -> Type -> Doc ann
prty p ty
        | fromEnum p >= fromEnum (precOf ty) = parens (pretty ty)
        | otherwise = pretty ty