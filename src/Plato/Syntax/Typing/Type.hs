module Plato.Syntax.Typing.Type where

import Data.IORef (IORef)
import Prettyprinter

import Plato.Common.Location
import Plato.Common.Name.Global
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Kind

-----------------------------------------------------------
-- Datas and types
-----------------------------------------------------------
type LType = Located Type

type Quant = (TyVar, Maybe Kind)

data Type
        = VarT TyVar
        | ConT GlbName
        | ArrT LType LType
        | AllT [Quant] LType
        | AppT LType LType
        | AbsT LName (Maybe Kind) LType
        | RecT LName Kind LType
        | RecordT [(GlbName, LType)]
        | SumT [(LName, [LType])]
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv LName
        | SkolemTv LName Uniq
        deriving (Show, Ord)

unTyVar :: TyVar -> LName
unTyVar (BoundTv x) = x
unTyVar (SkolemTv x _) = x

data MetaTv = Meta Uniq TyRef

type TyRef = IORef (Maybe Tau)

-----------------------------------------------------------
-- Basic instances
-----------------------------------------------------------
instance Eq TyVar where
        (BoundTv s1) == (BoundTv s2) = unLoc s1 == unLoc s2
        (SkolemTv _ u1) == (SkolemTv _ u2) = u1 == u2
        _ == _ = False

instance Eq MetaTv where
        (Meta u1 _) == (Meta u2 _) = u1 == u2

instance Show MetaTv where
        show (Meta u _) = "$" ++ show u

instance Ord MetaTv where
        Meta u1 _ `compare` Meta u2 _ = u1 `compare` u2

-----------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------
instance Pretty TyVar where
        pretty (BoundTv n) = pretty n
        pretty (SkolemTv n _) = pretty n

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec (unLoc arg)
        pretty (ArrT arg res) = pprty ArrPrec (unLoc arg) <+> "->" <+> pprty TopPrec (unLoc res)
        pretty (AllT vars body) = lbrace <> hsep (map (pretty . fst) vars) <> rbrace <+> pretty body
        pretty (AbsT var mkn body) = sep [backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mkn] <> dot <+> pretty body
        pretty (RecT var _ body) = "μ" <> pretty var <> dot <+> pretty body
        pretty (RecordT fields) =
                hsep
                        [ lbrace
                        , concatWith (\d -> (<+> comma <+> d)) (map (\(var, exp) -> pretty var <+> colon <+> pretty exp) fields)
                        , rbrace
                        ]
        pretty (SumT fields) =
                langle
                        <> concatWith
                                (\d e -> d <+> pipe <+> e)
                                ( map
                                        (\(c, tys) -> hsep (pretty c : map (pprty AppPrec . unLoc) tys))
                                        fields
                                )
                        <> rangle
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