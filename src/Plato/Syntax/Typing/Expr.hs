module Plato.Syntax.Typing.Expr where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Name.Global
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

-----------------------------------------------------------
-- Datas and types
-----------------------------------------------------------
type LExpr = Located Expr

data Expr
        = VarE GlbName
        | AppE Expr Expr
        | AbsE LName (Maybe Type) Expr
        | TAppE Expr [Type]
        | TAbsE [LName] Expr
        | LetE [FuncD] Expr
        | ProjE Expr GlbName
        | RecordE [(GlbName, Expr)]
        | CaseE Expr (Maybe Type) [(Pat, Expr)]
        | TagE GlbName [Expr] Type
        | FoldE Type
        | AnnE Expr Type
        deriving (Eq, Show)

data FuncD = FuncD LName Expr Type deriving (Eq, Show)

-----------------------------------------------------------
-- Pretty printing
-----------------------------------------------------------
instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty (AppE (FoldE ty) exp) = "fold" <+> lbracket <> pretty ty <> rbracket <+> pprexpr exp
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = hsep (pretty fun : map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE decs body) = hsep ["let", lbrace <> line, indent 4 (vsep (map pretty decs)), line <> rbrace, "in", pretty body]
        pretty (ProjE exp lab) = surround "." (pretty exp) (pretty lab)
        pretty (RecordE fields) =
                hsep
                        [ lbrace
                        , concatWith (\d -> (<+> comma <+> d)) (map (\(var, exp) -> pretty var <+> equals <+> pretty exp) fields)
                        , rbrace
                        ]
        pretty (CaseE match mty alts) =
                "case"
                        <+> sep [pretty match, colon, maybe emptyDoc pretty mty]
                        <+> "of"
                        <+> lbrace
                                <> line
                                <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                                <> line
                                <> rbrace
        pretty (TagE con args _) = hsep (pretty con : map pprexpr args)
        pretty (FoldE ty) = sep [lbracket, pretty ty, rbracket]
        pretty (AnnE exp ty) = pprexpr exp <+> colon <+> pretty ty

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e@(TagE _ as _) | null as = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk e1 (e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)

instance Pretty FuncD where
        pretty (FuncD var body body_ty) = hsep [pretty var, equals, pretty body, colon, pretty body_ty]
