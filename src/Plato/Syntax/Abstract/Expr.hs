{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Plato.Syntax.Abstract.Expr where

import qualified Data.Kind
import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import Plato.Syntax.Abstract.Classes
import Plato.Syntax.Abstract.Kind
import {-# SOURCE #-} Plato.Syntax.Abstract.Module
import Plato.Syntax.Abstract.Pat
import Plato.Syntax.Abstract.Type

----------------------------------------------------------------
-- Datas ans types
----------------------------------------------------------------
type LExpr a = Located (Expr a)

data Expr :: Data.Kind.Type -> Data.Kind.Type where
        VarE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                Path ->
                Expr a
        AppE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                LExpr a ->
                LExpr a ->
                Expr a
        AbsE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                Ident ->
                Maybe (Type a) ->
                LExpr a ->
                Expr a
        PAbsE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                LPat ->
                Maybe (Type a) ->
                LExpr a ->
                Expr a
        TAppE ::
                (CTyping a, CTyped a) =>
                LExpr a ->
                [Type a] ->
                Expr a
        TAbsE ::
                (CTyping a, CTyped a) =>
                [(TyVar a, Maybe (Kind a))] ->
                LExpr a ->
                Expr a
        BinOpE ::
                (CParsing a, CResolved a) =>
                LExpr a ->
                Path ->
                LExpr a ->
                Expr a
        LetE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                [Decl a] ->
                LExpr a ->
                Expr a
        CaseE ::
                (CParsing a, CResolved a, CTyping a, CTyped a) =>
                LExpr a ->
                Maybe (Type a) ->
                [(LPat, LExpr a)] ->
                Expr a
        FactorE ::
                CParsing a =>
                LExpr a ->
                Expr a

----------------------------------------------------------------
-- Class instances
----------------------------------------------------------------
instance Eq (Expr a)
instance Show (Expr a)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty (Expr a) where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (PAbsE pat mty body) = backslash <> pretty pat <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = pretty fun <> hsep (map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (BinOpE lhs op rhs) = pretty lhs <+> pretty op <+> pretty rhs
        pretty (LetE decs body) = hsep ["let", lbrace <> line, indent 4 (pretty decs), line <> rbrace, "in", pretty body] -- pretty binds
        pretty (CaseE match mty alts) =
                "case" <+> sep [pretty match, colon, maybe emptyDoc pretty mty] <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (FactorE exp) = parens $ pretty exp

pprexpr :: Expr a -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr a -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr a -> [Expr a] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)