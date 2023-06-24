module Plato.Driver.Context where

import Plato.Common.Uniq
import Plato.Core.Data as C
import Plato.Nicifier.OpParser.Fixity
import Plato.PsToTyp.Scoping as S
import Plato.Typing.Env

data Contexts = Contexts
        { uniqs :: Uniq
        , fixityEnv :: FixityEnv
        , scope :: S.Scope
        , typEnv :: TypEnv
        , coreEnv :: EnvEntries
        , coreScope :: C.Scope
        }