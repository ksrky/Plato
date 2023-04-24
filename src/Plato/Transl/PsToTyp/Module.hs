module Plato.Transl.PsToTyp.Module where

import Control.Exception.Safe

import qualified Plato.Syntax.Parsing as P
import qualified Plato.Syntax.Typing as T

elabModule :: MonadThrow m => P.Program -> m T.Module
elabModule = undefined