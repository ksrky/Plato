module Plato.Syntax.Parsing.Expr where

import Prettyprinter

import Plato.Common.Location
import Plato.Common.Path
import {-# SOURCE #-} Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Pat

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr

data Expr
        = VarE Path
        | AppE LExpr LExpr
        | OpE LExpr Path LExpr
        | LamE [([LPat], LExpr)]
        | LetE [LDecl] LExpr
        | CaseE LExpr [(LPat, LExpr)]
        | FactorE LExpr -- removed after fixity resolution
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Expr where
        substPath (VarE path) = VarE <$> substPath path
        substPath (AppE fun arg) = AppE <$> substPath `traverse` fun <*> substPath `traverse` arg
        substPath (OpE lhs op rhs) =
                OpE <$> substPath `traverse` lhs <*> substPath op
                        <*> substPath `traverse` rhs
        substPath (LamE alts) =
                LamE
                        <$> mapM
                                ( \(pats, body) ->
                                        (,)
                                                <$> mapM (substPath `traverse`) pats
                                                        <*> substPath `traverse` body
                                )
                                alts
        substPath (LetE decs body) =
                LetE <$> mapM (substPath `traverse`) decs
                        <*> substPath `traverse` body
        substPath (CaseE match alts) = do
                alts' <-
                        mapM
                                ( \(pat, exp) ->
                                        (,) <$> substPath `traverse` pat
                                                <*> substPath `traverse` exp
                                )
                                alts
                CaseE <$> substPath `traverse` match <*> return alts'
        substPath (FactorE exp) = FactorE <$> substPath `traverse` exp

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (OpE lhs op rhs) = pretty lhs <+> pretty op <+> pretty rhs
        pretty (LamE alts) =
                backslash <> case alts of
                        [(pats, body)] -> ppralt (pats, body)
                        _ -> vcat ["where", indent 4 (vsep $ map ppralt alts)]
            where
                ppralt :: ([LPat], LExpr) -> Doc ann
                ppralt (pats, body) = hsep (map pretty pats) <+> "->" <+> pretty body
        pretty (LetE decs body) =
                "let" <+> lbrace <> line
                        <> indent 4 (vsep (map pretty decs))
                        <> line
                        <> rbrace <+> "in" <+> pretty body
        pretty (CaseE match alts) =
                "case" <+> pretty match <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace
        pretty (FactorE exp) = pretty exp

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprParendExpr e' <+> sep (map pprParendExpr es)
        pprParendExpr :: Expr -> Doc ann
        pprParendExpr e@VarE{} = pretty e
        pprParendExpr e = parens (pretty e)
