{-# LANGUAGE OverloadedStrings #-}

module Plato.Utils.PrettyParsing where

import Plato.Common.SrcLoc
import Plato.Syntax.Parsing

import Prettyprinter

instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@(AppE fun arg) = pprapp exp
        pretty (OpE lhs op rhs) = pretty lhs <+> pretty rhs
        pretty (LamE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE decs body) = sep ["let", lbrace, line, indent 4 (vsep (map pretty decs)), line, rbrace, "in", pretty body]
        pretty (CaseE match alts) =
                "case" <+> pretty match <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (FactorE exp) = pretty (unLoc exp)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprParendExpr e' <+> sep (map pprParendExpr es)
        pprParendExpr :: Expr -> Doc ann
        pprParendExpr e@VarE{} = pretty e
        pprParendExpr e = parens (pretty e)

instance Pretty Pat where
        pretty (ConP con pats) = pretty con <+> hsep (map pprpat pats)
        pretty (VarP var) = pretty var
        pretty WildP = "_"

pprpat :: Located Pat -> Doc ann
pprpat pat@(L _ (ConP con pats))
        | null pats = pretty con
        | otherwise = parens $ pretty pat
pprpat pat = pretty pat

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

pprty :: Prec -> Located Type -> Doc ann
pprty p (L _ ty)
        | fromEnum p >= fromEnum (precty ty) = parens (pretty ty)
        | otherwise = pretty ty

instance Pretty Decl where
        pretty (FuncD var args body) = pretty var <+> hsep (map pretty args) <+> equals <+> pretty body
        pretty (FuncTyD var body_ty) = pretty var <+> colon <+> pretty body_ty

instance Pretty TopDecl where
        pretty (DataD con args fields) =
                pretty con <+> hsep (map pretty args) <+> equals
                        <+> concatWith (surround pipe) (map (\(c, tys) -> pretty c <+> hsep (map pretty tys)) fields)
        pretty (TypeD con args body) = pretty con <+> hsep (map pretty args) <+> equals <+> pretty body
        pretty FixD = emptyDoc
        pretty (Decl dec) = pretty dec
        pretty (Eval exp) = pretty exp

instance Pretty Program where
        pretty (Program mod imps topdecs) =
                maybe emptyDoc pretty mod <> line
                        <> vsep (map (\imp -> "import" <+> pretty imp) imps)
                        <> vsep (map pretty topdecs)