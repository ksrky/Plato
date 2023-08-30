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

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Pretty
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
        | FreeTv {unTyVar :: Ident}
        deriving (Ord)

data MetaTv = MetaTv Uniq (IORef (Maybe Tau))

----------------------------------------------------------------
-- Basic interfaces
----------------------------------------------------------------
instance Show TyVar where
        show (BoundTv id) = "(BoundTv " ++ show id ++ ")"
        show (FreeTv id) = "(FreeTv " ++ show id ++ ")"

instance Eq TyVar where
        (BoundTv id1) == (BoundTv id2) = id1 == id2
        (FreeTv id1) == (FreeTv id2) = id1 == id2
        _ == _ = False

instance Eq MetaTv where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Show MetaTv where
        show (MetaTv u _) = show u

instance Ord MetaTv where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty TyVar where
        pretty (BoundTv id) = pretty id
        pretty (FreeTv id) = pretty id

instance Pretty MetaTv where
        pretty (MetaTv u _) = dollar <> pretty u

prQuant :: Quant -> Doc ann
prQuant (tv, kn) = hsep [pretty tv, colon, pretty kn]

prQuants :: [Quant] -> Doc ann
prQuants [qnt] = prQuant qnt
prQuants qnts = hsep $ map (parens . prQuant) qnts

instance Pretty Type where
        pretty = pretty' 0

instance PrettyWithContext Type where
        pretty' _ (VarT tv) = pretty tv
        pretty' _ (ConT tc) = pretty tc
        pretty' c (ArrT arg res) = contextParens c 0 $ hsep [pretty' 1 arg, arrow, pretty res]
        pretty' c (AllT [] body) = pretty' c body
        pretty' c (AllT qnts body) = contextParens c 0 $ hsep [braces (prQuants qnts), pretty body]
        pretty' c (AppT fun arg) = contextParens c 1 $ pretty' 1 fun <+> pretty' 2 arg
        pretty' _ (MetaT tv) = pretty tv
