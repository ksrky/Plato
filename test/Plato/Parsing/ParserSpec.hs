module Plato.Parsing.ParserSpec where

import Control.Monad.Reader
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Parsing
import Plato.Parsing.Monad
import Plato.Parsing.Parser

spec :: Spec
spec = do
        describe "Parsing expression" $ do
                it "lambda abstraction" $ do
                        pending -- parseExpr "\\x -> x\n" `shouldReturn` "\\x -> x"
                it "function application" $ do
                        pending -- parseExpr "f x y\n" `shouldReturn` "f x y"
                it "multiple lambda abstraction" $ do
                        pending -- parseExpr "\\x y z -> x\n" `shouldReturn` "\\x y z -> x"
        describe "Parsing a file" $ do
                it "test02.plt" $ do
                        parseProg "test02.plt" `shouldReturn` "ok"

parseExpr :: T.Text -> IO String
parseExpr inp = do
        exp <- parsePartial inp exprParser
        return $ show $ pretty exp

parseProg :: FilePath -> IO String
parseProg fn = do
        inp <- T.readFile ("test/testcases/" ++ fn)
        (ast, _) <- liftIO $ parse fn inp parser
        return "ok"