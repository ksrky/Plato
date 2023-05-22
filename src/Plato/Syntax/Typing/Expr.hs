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
        | CaseE LExpr [(LPat, LExpr)]
        | ClauseE (Maybe [Type]) [Clause]
        deriving (Eq, Show)

data FunDecl
        = FunBind Ident [LPat] LExpr
        | FunSpec Ident LType
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Expr

instance Pretty FunDecl