module Plato.Parsing.LexerSpec where

import Control.Monad.Reader
import Data.Text qualified as T
import Test.Hspec

import Plato.Common.Uniq
import Plato.Parsing
import Plato.Parsing.Parser
import Plato.Parsing.Token

spec :: Spec
spec = do
        describe "line test" $ do
                it "varid" $ do
                        parseTokens "x" `shouldReturn` [TokVarId "x"]
                it "conid" $ do
                        parseTokens "True" `shouldReturn` [TokConId "True"]
                it "varsym" $ do
                        parseTokens "x + y" `shouldReturn` [TokVarId "x", TokVarSym "+", TokVarId "y"]
                it "consym" $ do
                        parseTokens "True :: False" `shouldReturn` [TokConId "True", TokConSym "::", TokConId "False"]

semi :: Token
semi = TokSymbol SymSemicolon

parseTokens :: T.Text -> IO [Token]
parseTokens inp = runReaderT (parsePartial inp tokenParser) =<< initUniq