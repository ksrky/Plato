module Plato.Syntax.Typing.Type where

import Data.IORef (IORef)
import Prettyprinter

import Plato.Common.Global
import Plato.Common.Ident as Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Typing.Kind

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LType = Located Type

type Quant = (TyVar, Maybe Kind)

data Type
        = VarT TyVar
        | ConT Path
        | ArrT LType LType
        | AllT [Quant] (Located Rho)
        | AppT LType LType
        | AbsT Ident (Maybe Kind) LType
        | --  | RecordT [(Ident, LType)]
          --  | SumT [(Ident, [LType])]
          MetaT MetaTv
        deriving (Eq, Show)

type Sigma = Type
type Rho = Type
type Tau = Type

data TyVar
        = BoundTv {unTyVar :: Ident}
        | SkolemTv {unTyVar :: Ident}
        deriving (Show, Ord)

data MetaTv = MetaTv Unique (IORef (Maybe Tau))

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

instance Substitutable Type where
        substPath (VarT tv) = return $ VarT tv
        substPath (ConT tc) = ConT <$> substPath tc
        substPath (ArrT arg res) = ArrT <$> substPath `traverse` arg <*> substPath `traverse` res
        substPath (AllT qnts body) = AllT qnts <$> substPath `traverse` body
        substPath (AppT fun arg) = AppT <$> substPath `traverse` fun <*> substPath `traverse` arg
        substPath (AbsT var mkn body) = AbsT var <$> substPath `traverse` mkn <*> substPath `traverse` body
        substPath (MetaT mtv) = return $ MetaT mtv

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
        pretty (AllT vars body) = lbrace <> hsep (map (pretty . fst) vars) <> rbrace <+> pretty body
        pretty (AppT fun arg) = pretty fun <+> pprty AppPrec (unLoc arg)
        pretty (AbsT var mkn body) = sep [backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mkn] <> dot <+> pretty body
        -- pretty (RecT var _ body) = "μ" <> pretty var <> dot <+> pretty body
        {-pretty (RecordT fields) =
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
                        <> rangle-}
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