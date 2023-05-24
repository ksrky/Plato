module Plato.Syntax.Parsing (
        module Plato.Syntax.Parsing.Expr,
        module Plato.Syntax.Parsing.Decl,
        module Plato.Syntax.Parsing.Pat,
        module Plato.Syntax.Parsing.TopDecl,
        module Plato.Syntax.Parsing.Type,
        Program,
) where

import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.TopDecl
import Plato.Syntax.Parsing.Type

type Program = [LTopDecl]
