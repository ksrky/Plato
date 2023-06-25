{-# LANGUAGE ImpredicativeTypes #-}

module Plato.Syntax.Parsing (
        module Plato.Syntax.Parsing.Expr,
        module Plato.Syntax.Parsing.Decl,
        module Plato.Syntax.Parsing.Pat,
        module Plato.Syntax.Parsing.Type,
        Instr (..),
        LInstr,
        Program,
) where

import Data.Text qualified as T

import Plato.Common.Location (Located)
import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

data Instr
        = ImpDecl (Located T.Text)
        | TopDecls [LTopDecl]
        | EvalExpr LExpr
        deriving (Eq, Show)

type LInstr = Located Instr

type Program = [LInstr]
