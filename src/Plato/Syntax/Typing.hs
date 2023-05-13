module Plato.Syntax.Typing (
        module Plato.Syntax.Typing.Base,
        module Plato.Syntax.Typing.Decl,
        module Plato.Syntax.Typing.Expr,
        module Plato.Syntax.Typing.Kind,
        module Plato.Syntax.Typing.Pat,
        module Plato.Syntax.Typing.Type,
        Program (..),
        TyEnv,
        KnEnv,
) where

import Data.Map.Strict qualified as M
import Prettyprinter

import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Syntax.Typing.Base
import Plato.Syntax.Typing.Decl
import Plato.Syntax.Typing.Expr
import Plato.Syntax.Typing.Kind
import Plato.Syntax.Typing.Pat
import Plato.Syntax.Typing.Type

data Program = Program
        { typ_modn :: ModuleName
        , typ_decls :: [LDecl]
        , typ_binds :: [FuncD]
        , typ_body :: [(LExpr, Type)]
        }
        deriving (Eq, Show)

type TyEnv = M.Map GlbName Sigma
type KnEnv = M.Map GlbName Kind

instance Pretty Program where
        pretty (Program mod decs binds body) =
                pretty mod
                        <> line
                        <> vsep (map pretty decs)
                        <> (if null decs then emptyDoc else line)
                        <> vsep (map pretty binds)
                        <> (if null binds then emptyDoc else line)
                        <> vsep (map (pretty . fst) body)