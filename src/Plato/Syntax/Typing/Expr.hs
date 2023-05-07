module Plato.Syntax.Typing.Expr where

import Data.List.NonEmpty (NonEmpty)
import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Common.Path
import {-# SOURCE #-} Plato.Syntax.Typing.Module
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr

data Expr
        = VarE Path
        | AppE LExpr LExpr
        | AbsE Ident (Maybe Type) LExpr
        | PAbsE LPat (Maybe Type) LExpr
        | TAppE LExpr [Type]
        | TAbsE [Quant] LExpr
        | -- | `Decl` does not include data & module declaration because of the avoidance problem
          LetE [Decl] LExpr
        | MatchE (NonEmpty (LExpr, Maybe Type)) [(NonEmpty LPat, LExpr)]
        deriving (Eq, Show)

----------------------------------------------------------------
-- Basic instances
----------------------------------------------------------------
instance Substitutable Expr where
        substPath (VarE path) = VarE <$> substPath path
        substPath (AppE fun arg) = AppE <$> substPath `traverse` fun <*> substPath `traverse` arg
        substPath (AbsE var mty body) = AbsE var <$> substPath `traverse` mty <*> substPath `traverse` body
        substPath (PAbsE pat mty body) = PAbsE pat <$> substPath `traverse` mty <*> substPath `traverse` body
        substPath (TAppE fun tyargs) = TAppE <$> substPath `traverse` fun <*> mapM substPath tyargs
        substPath (TAbsE qnts body) = TAbsE qnts <$> substPath `traverse` body
        substPath (LetE decs body) = LetE <$> substPath `traverse` decs <*> substPath `traverse` body
        substPath (MatchE matches alts) = do
                alts' <- mapM (\(pats, exp) -> (,) <$> mapM (substPath `traverse`) pats <*> substPath `traverse` exp) alts
                MatchE <$> mapM (\(e, mty) -> (,) <$> substPath `traverse` e <*> substPath `traverse` mty) matches
                        <*> return alts'

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr where
        pretty (VarE var) = pretty var
        pretty exp@AppE{} = pprapp exp
        pretty (AbsE var mty body) = backslash <> pretty var <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (PAbsE pat mty body) = backslash <> pretty pat <> maybe emptyDoc ((colon <>) . pretty) mty <> dot <+> pretty body
        pretty (TAppE fun tyargs) = pretty fun <> hsep (map pretty tyargs)
        pretty (TAbsE vars body) = backslash <> hsep (map pretty vars) <> dot <+> pretty body
        pretty (LetE decs body) = hsep ["let", lbrace <> line, indent 4 (pretty decs), line <> rbrace, "in", pretty body] -- pretty binds
        pretty (MatchE matches alts) =
                "case" <+> sep [pretty matches, colon] <+> "of" <+> lbrace <> line
                        <> indent 4 (vsep (map (\(pat, body) -> pretty pat <+> "->" <+> pretty body) alts))
                        <> line
                        <> rbrace

-- pretty (PBarE lhs rhs) = hsep [pretty lhs, pipe, pretty rhs]

pprexpr :: Expr -> Doc ann
pprexpr e@VarE{} = pretty e
pprexpr e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = pprexpr e' <+> sep (map pprexpr es)