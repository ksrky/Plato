module Plato.Transl.SrcToPs where

import Plato.Common.Error
import Plato.Parsing.FixResol
import Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Parsing

import Control.Exception.Safe (MonadThrow)
import qualified Data.Text as T

src2ps :: MonadThrow m => T.Text -> m Program
src2ps inp = do
        (res, st) <- eitherToMonadThrow (parse inp parser)
        let opdict = opDict (parser_ust st)
        resolveFixity opdict res

parseLine :: T.Text -> ParserT m a -> m (a, PsState)
parseLine inp p = parse inp (ParserT $ \st -> runParserT p st{parser_scd = code})