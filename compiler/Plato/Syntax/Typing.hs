{-# LANGUAGE DataKinds #-}

module Plato.Syntax.Typing (
        module Plato.Syntax.Typing.Base,
        module Plato.Syntax.Typing.Decl,
        module Plato.Syntax.Typing.Expr,
        module Plato.Syntax.Typing.Kind,
        module Plato.Syntax.Typing.Pat,
        module Plato.Syntax.Typing.Type,
        Program,
        Prog (..),
) where

import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Decl
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

type Program a = [Decl a]

data Prog (a :: TcFlag) = Prog
        { p_typdefns :: [TypDefn a]
        , p_binds :: [Bind a]
        }