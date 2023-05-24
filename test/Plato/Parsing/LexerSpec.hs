module Plato.Parsing.LexerSpec where

import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Hspec

import Plato.Common.Location
import Plato.Parsing
import Plato.Parsing.Monad
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
                        parseTokens "True :: False" `shouldReturn` [TokVarId "x"]

parseTokensFile :: FilePath -> IO [Token]
parseTokensFile fn = do
        inp <- T.readFile ("test/testcases/" ++ fn)
        (toks, _) <- liftIO $ parse fn inp tokenParser
        return $ map unLoc toks

parseTokens :: T.Text -> IO [Token]
parseTokens inp = do
        toks <- liftIO $ parsePartial inp tokenParser
        return $ map unLoc toks