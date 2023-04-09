module Plato.Syntax.Typing.Expr where

import Prettyprinter

import Plato.Common.Location
import Plato.Syntax.Typing.Ident
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Path
import Plato.Syntax.Typing.Type

type LExpr = Located Expr

data Expr
        = VarE Path
        | AppE LExpr LExpr
        | AbsE Ident (Maybe Type) LExpr
        | PAbsE LPat (Maybe Type) LExpr
        | TAppE LExpr [Type]
        | TAbsE [(TyVar, Maybe Kind)] LExpr
        | LetE [(Ident, Maybe LType, LExpr)] LExpr
        | CaseE LExpr (Maybe Type) [(LPat, LExpr)]
        | PBarE LExpr LExpr
        deriving (Eq, Show)

instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (PAbsE pat mty body) = backslash <> pretty pat <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = pretty fun <> hsep (map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE binds body) = hsep ["let", lbrace <> line, indent 4 (pretty binds), line <> rbrace, "in", pretty body] -- pretty binds
        pretty (CaseE match mty alts) =
                "case" <+> sep [pretty match, colon, maybe emptyDoc pretty mty] <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (PBarE lhs rhs) = hsep [pretty lhs, pipe, pretty rhs]

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)