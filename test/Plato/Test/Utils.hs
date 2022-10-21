module Plato.Test.Utils where

import Plato.Parsing.Lexer
import Plato.Parsing.Monad

import qualified Data.Text as T
import Test.Hspec

parseLine :: T.Text -> ParserT m a -> m (a, PsState)
parseLine inp p = parse inp (ParserT $ \st -> runParserT p st{parser_scd = code})

shouldSatisfyReturn :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyReturn` check = action >>= (`shouldSatisfy` check)