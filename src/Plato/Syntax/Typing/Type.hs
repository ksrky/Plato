module Plato.Syntax.Typing.Type where

import Data.IORef (IORef)
import Plato.Typing.Subst
import Prettyprinter

import Plato.Common.Global
import Plato.Common.Location 
import Plato.Syntax.Typing.Ident as Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Path

type LType = Located Type

data Type
        = VarT TyVar
        | ConT Path
        | ArrT LType LType
        | AllT [(TyVar, Maybe Kind)] (Located Rho)
        | AppT LType LType
        | AbsT Ident (Maybe Kind) LType
        | RecordT [(Ident, LType)]
        | SumT [(Ident, [LType])]
        | MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv {unTyVar :: Ident}
        | SkolemTv {unTyVar :: Ident}
        deriving (Show, Ord)

data MetaTv = MetaTv Unique (IORef (Maybe Tau))

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

instance Substitutable Type where
        subst _ _ = undefined

instance Pretty TyVar where
        pretty (BoundTv id) = pretty id
        pretty (SkolemTv id) = pretty id

instance Pretty Type where
        pretty (VarT var) = pretty var
        pretty (ConT con) = pretty con
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec (unLoc arg)
        pretty (ArrT arg res) = pprty ArrPrec (unLoc arg) <+> "->" <+> pprty TopPrec (unLoc res)
        pretty (AllT vars body) = lbrace <> hsep (map (pretty . fst) vars) <> rbrace <+> pretty body
        pretty (AbsT var mkn body) = sep [backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mkn] <> dot <+> pretty body
        -- pretty (RecT var _ body) = "μ" <> pretty var <> dot <+> pretty body
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
                                        (\(c, tys) -> pretty c <> hsep (map (pprty AppPrec . unLoc) tys))
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