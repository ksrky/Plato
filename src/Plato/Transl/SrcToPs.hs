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

src2ps :: MonadThrow m => OpTable -> T.Text -> m (OpTable, Program)
src2ps optab inp = do
        (res, st) <- eitherToMonadThrow (parse inp parser)
        let optab' = optab `M.union` opTable (parser_ust st)
        (optab',) <$> resolveFixity optab' res

parseLine :: ParserT m a -> T.Text -> m (a, PsState)
parseLine p inp = parse inp (ParserT $ \st -> runParserT p st{parser_scd = code})

moduleName :: Program -> Maybe ModuleName
moduleName = (unLoc <$>) . Plato.Syntax.Parsing.moduleDecl

importModule :: Program -> [Located ModuleName]
importModule = Plato.Syntax.Parsing.importDecls