module Plato.Syntax.Typing.Type (
        LType,
        Quant,
        Type (..),
        TyVar (..),
        MetaTv (..),
        Sigma,
        Rho,
        Tau,
) where

import Data.IORef (IORef)
import Prettyprinter

import Plato.Common.Ident as Ident
import Plato.Common.Location
import Plato.Common.Uniq
import Plato.Syntax.Typing.Kind

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LType = Located Type

type Quant = (TyVar, Maybe Kind)

data Type
        = VarT TyVar
        | ConT Ident
        | ArrT LType LType
        | AllT [Quant] (Located Rho)
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv {unTyVar :: Ident}
        | SkolemTv {unTyVar :: Ident}
        deriving (Show, Ord)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

----------------------------------------------------------------
-- Basic interfaces
----------------------------------------------------------------
instance Eq TyVar where
        (BoundTv id1) == (BoundTv id2) = id1 == id2
        (SkolemTv id1) == (SkolemTv id2) = id1 == id2
        _ == _ = False

instance Eq MetaTv where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Show MetaTv where
        show (MetaTv u _) = "$" ++ show u

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty TyVar where
        pretty (BoundTv id) = pretty id
        pretty (SkolemTv id) = pretty id

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (ArrT arg res) = pprty ArrPrec (unLoc arg) <+> "->" <+> pprty TopPrec (unLoc res)
        pretty (AllT qnts body) = lbrace <> hsep (map (pretty . fst) qnts) <> rbrace <+> pretty body
        pretty (MetaT tv) = viaShow tv

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precty :: Type -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> Type -> Doc ann
pprty p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty