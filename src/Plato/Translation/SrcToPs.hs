module Plato.Translation.SrcToPs where

import Plato.Common.Error
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Parsing.Resolver
import Plato.Syntax.Parsing

import Control.Exception.Safe (MonadThrow)
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> m Program
src2ps inp = do
        (res, st) <- eitherToMonadThrow (parse inp parser)
        let opdict = opDict (parser_ust st)
        resolveFixity opdict res