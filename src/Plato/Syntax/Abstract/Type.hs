{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Plato.Syntax.Abstract.Type where

import Data.IORef (IORef)
import qualified Data.Kind
import Prettyprinter

import Plato.Common.Global
import Plato.Common.Location
import Plato.Common.Ident as Ident
import Plato.Syntax.Abstract.Kind
import Plato.Common.Path
import Plato.Syntax.Abstract.Classes

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LType a = Located (Type a)

data Type :: Data.Kind.Type -> Data.Kind.Type where
        VarT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                TyVar a ->
                Type a
        ConT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                Path ->
                Type a
        ArrT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                LType a ->
                LType a ->
                Type a
        AllT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                [(TyVar a, Maybe (Kind a))] ->
                LType a ->
                Type a
        AppT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                LType a ->
                LType a ->
                Type a
        AbsT ::
                (CTyping a, CTyped a) =>
                Ident ->
                Maybe (Kind a) ->
                LType a ->
                Type a
        RecordT ::
                (CTyping a, CTyped a) =>
                [(Ident, LType a)] ->
                Type a
        SumT ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                [(Ident, [LType a])] ->
                Type a
        MetaT ::
                CTyping a =>
                MetaTv a ->
                Type a

-- type Sigma = Type Typing
-- type Rho = Type Typing
-- type Tau = Type Typing

data TyVar :: Data.Kind.Type -> Data.Kind.Type where
        BoundTv ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                {unTyVar :: Ident} ->
                TyVar a
        SkolemTv ::
                (CTyping a, CTyped a) =>
                {unTyVar :: Ident} ->
                TyVar a

data MetaTv :: Data.Kind.Type -> Data.Kind.Type where
        MetaTv ::
                CTyping a =>
                Unique ->
                IORef (Maybe (Type a)) ->
                MetaTv a

----------------------------------------------------------------
-- Class instances
----------------------------------------------------------------
instance Eq (TyVar a) where
        (BoundTv id1) == (BoundTv id2) = id1 == id2
        (SkolemTv id1) == (SkolemTv id2) = id1 == id2
        _ == _ = False

instance Show (TyVar a) where
        show (BoundTv id) = "BoundTv " ++ show id
        show (SkolemTv id) = "SkolemTv " ++ show id

instance Ord (TyVar a) where
        BoundTv id1 `compare` BoundTv id2 = id1 `compare` id2
        SkolemTv id1 `compare` SkolemTv id2 = id1 `compare` id2
        BoundTv id1 `compare` SkolemTv id2 = id1 `compare` id2
        SkolemTv id1 `compare` BoundTv id2 = id1 `compare` id2

instance Eq (MetaTv a) where
        (MetaTv u1 _) == (MetaTv u2 _) = u1 == u2

instance Show (MetaTv a) where
        show (MetaTv u _) = "$" ++ show u

instance Ord (MetaTv a) where
        MetaTv u1 _ `compare` MetaTv u2 _ = u1 `compare` u2

instance Substitutable (Type a) where
        subst _ _ = undefined

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (TyVar a) where
        pretty (BoundTv id) = pretty id
        pretty (SkolemTv id) = pretty id

instance Pretty (Type a) where
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

precty :: Type a -> Prec
precty AllT{} = TopPrec
precty ArrT{} = ArrPrec
precty _ = AtomPrec

pprty :: Prec -> Type a -> Doc ann
pprty p ty
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty