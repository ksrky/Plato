module Plato.Syntax.Parsing.Type where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path

----------------------------------------------------------------
-- Data and type
----------------------------------------------------------------
type LType = Located Type

data Type
        = VarT Ident
        | ConT Path
        | AppT LType LType
        | ArrT LType LType
        | AllT [Ident] LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Type where
        substPath (VarT id) = return $ VarT id
        substPath (ConT path) = ConT <$> substPath path
        substPath (AppT fun arg) = AppT <$> substPath `traverse` fun <*> substPath `traverse` arg
        substPath (ArrT arg res) = ArrT <$> substPath `traverse` arg <*> substPath `traverse` res
        substPath (AllT qnts body) = AllT qnts <$> substPath `traverse` body

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec arg
        pretty (ArrT arg res) = pprty ArrPrec arg <+> "->" <+> pprty TopPrec res
        pretty (AllT vars body) = lbrace <> hsep (map pretty vars) <> rbrace <+> pretty body

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> LType -> Doc ann
pprty p (L _ ty)
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty