module Plato.Syntax.Typing.Type (
        LType,
        Quant,
        Type (..),
        TyVar (..),
        MetaTv (..),
        Sigma,
        Rho,
        Tau,
        prQuant,
        prQuants,
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

type Quant = (TyVar, Kind)

data Type
        = VarT TyVar
        | ConT Ident
        | ArrT LType LType
        | AllT [Quant] (Located Rho)
        | AppT LType LType
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type -- ∀α. ρ
type Rho = Type -- σ → ρ
type Tau = Type -- τ

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
        show (MetaTv u _) = "MetaTv " ++ show u

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty TyVar where
        pretty (BoundTv id) = pretty id
        pretty (SkolemTv id) = pretty (nameIdent id) <> pretty (stamp id)

instance Pretty MetaTv where
        pretty (MetaTv u _) = "$" <> pretty u

prQuant :: Quant -> Doc ann
prQuant (tv, kn) = hcat [pretty tv, colon, pretty kn]

prQuants :: [Quant] -> Doc ann
prQuants [(tv, kn)] = hcat [pretty tv, colon, pretty kn]
prQuants qnts = hsep $ map (parens . prQuant) qnts

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (ArrT arg res) = hsep [prty ArrPrec (unLoc arg), "->", prty TopPrec (unLoc res)]
        pretty (AllT qnts body) = hsep [braces (prQuants qnts), pretty body]
        pretty (AppT fun arg) = pretty fun <+> prty AppPrec (unLoc arg)
        pretty (MetaT tv) = pretty tv

data Prec = TopPrec | ArrPrec | AppPrec | AtomPrec deriving (Enum)

precOf :: Type -> Prec
precOf AllT{} = TopPrec
precOf ArrT{} = ArrPrec
precOf _ = AtomPrec

prty :: Prec -> Type -> Doc ann
prty p ty
        | fromEnum p >= fromEnum (precOf ty) = parens (pretty ty)
        | otherwise = pretty ty