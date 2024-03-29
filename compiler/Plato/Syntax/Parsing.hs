module Plato.Syntax.Parsing (
        module Plato.Syntax.Parsing.Expr,
        module Plato.Syntax.Parsing.Decl,
        module Plato.Syntax.Parsing.Pat,
        module Plato.Syntax.Parsing.Type,
        Program,
        Instr(..),
) where

import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

type Program = [LTopDecl]

data Instr
        = InstrEval LExpr
        | InstrDecls [LTopDecl]