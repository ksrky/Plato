module Plato.Syntax.Typing.Type (
        LType,
        Quant,
        Quants,
        Type (..),
        TyVar (..),
        MetaTv (..),
        Sigma,
        Rho,
        Tau,
        prQuant,
        prQuants,
) where

import Data.IORef

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Common.Pretty
import Plato.Common.Uniq
import Plato.Syntax.Typing.Kind

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LType = Located Type

type Quant = (TyVar, Kind)
type Quants = [Quant]

data Type
        = VarT TyVar
        | ConT Path
        | ArrT LType LType
        | AllT Quants (Located Rho)
        | AppT LType LType
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type -- ∀α. ρ
type Rho = Type -- σ → ρ
type Tau = Type -- τ

data TyVar
        = BoundTv Ident
        | FreeTv Ident
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

instance HasLoc TyVar where
        getLoc (BoundTv id) = getLoc id
        getLoc (FreeTv id) = getLoc id

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
        pretty' _ (VarT tv) = pretty tv
        pretty' _ (ConT tc) = pretty tc
        pretty' p (ArrT arg res) = parenswPrec p 0 $ hsep [pretty' 1 arg, arrow, pretty res]
        pretty' p (AllT [] body) = pretty' p body
        pretty' p (AllT qnts body) = parenswPrec p 0 $ hsep [braces (prQuants qnts), pretty body]
        pretty' p (AppT fun arg) = parenswPrec p 1 $ pretty' 1 fun <+> pretty' 2 arg
        pretty' _ (MetaT tv) = pretty tv
