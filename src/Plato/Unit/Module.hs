module Plato.Unit.Module where

import Plato.Common.Name
import Plato.Core.Context
import Plato.Parsing.Fixity
import Plato.Syntax.Typing

import qualified Data.Set as S

data Module = Module
        { mod_nameEnv :: ()
        , mod_opEnv :: OpTable
        , mod_typeEnv :: TypTable
        , mod_context :: Context
        , mod_deps :: S.Set ModuleName
        }