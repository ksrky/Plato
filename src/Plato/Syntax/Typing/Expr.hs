module Plato.Syntax.Typing.Expr (
        LExpr,
        Clause,
        Expr (..),
        FunDecl,
) where

import Prettyprinter

import Plato.Common.Ident
import Plato.Common.Location
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

----------------------------------------------------------------
-- Datas and types
----------------------------------------------------------------
type LExpr = Located Expr

type Clause = ([LPat], LExpr)

data Expr
        = VarE Ident
        | AppE LExpr LExpr
        | AbsE Ident (Maybe Type) LExpr
        | TAppE LExpr [Type]
        | TAbsE [Quant] LExpr
        | LetE [(Ident, LExpr)] [(Ident, Type)] LExpr
        | ClauseE [Clause]
        deriving (Eq, Show)

data FunDecl
        = FunBind Ident [LPat] LExpr
        | FunSpec Ident LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr

prettyAtom :: Expr -> Doc ann
prettyAtom e@VarE{} = pretty e
prettyAtom e = parens (pretty e)

pprapp :: Expr -> Doc ann
pprapp e = walk e []
    where
        walk :: Expr -> [Expr] -> Doc ann
        walk (AppE e1 e2) es = walk (unLoc e1) (unLoc e2 : es)
        walk e' es = prettyAtom e' <+> sep (map prettyAtom es)

instance Pretty FunDecl