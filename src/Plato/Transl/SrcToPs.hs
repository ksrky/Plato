{-# LANGUAGE TupleSections #-}

module Plato.Transl.SrcToPs where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc

import Plato.Parsing.FixResol
import Plato.Parsing.Fixity
import Plato.Parsing.Lexer
import Plato.Parsing.Monad
import Plato.Parsing.Parser

import Plato.Syntax.Parsing

import Control.Exception.Safe (MonadThrow)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

src2ps :: MonadThrow m => OpDict -> T.Text -> m (OpDict, [Located ModuleName], Program)
src2ps opdict inp = do
        (res, st) <- eitherToMonadThrow (parse inp parser)
        let opdict' = opdict `M.union` opDict (parser_ust st)
        (opdict',importDecls res,) <$> resolveFixity opdict' res

parseLine :: ParserT m a -> T.Text -> m (a, PsState)
parseLine p inp = parse inp (ParserT $ \st -> runParserT p st{parser_scd = code})
