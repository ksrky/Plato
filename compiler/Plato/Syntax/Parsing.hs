module Plato.Syntax.Parsing
    ( Command (..)
    , module Plato.Syntax.Parsing.Decl
    , module Plato.Syntax.Parsing.Expr
    , module Plato.Syntax.Parsing.Pat
    , module Plato.Syntax.Parsing.Type
    , Program
    ) where

import Plato.Syntax.Parsing.Decl
import Plato.Syntax.Parsing.Expr
import Plato.Syntax.Parsing.Pat
import Plato.Syntax.Parsing.Type

type Program = [LTopDecl]

data Command
    = CommandEval LExpr
    | CommandDecls [LTopDecl]
