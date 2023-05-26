module Plato.Parsing.ParserSpec where

import Control.Monad.Reader
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Parsing
import Plato.Parsing.Parser

spec :: Spec
spec = do
        describe "Parsing expression" $ do
                it "lambda abstraction" $ do
                        parseExpr "\\x -> x" `shouldReturn` "\\x -> x"
                it "function application" $ do
                        parseExpr "f x y" `shouldReturn` "f x y"
                it "multiple lambda abstraction" $ do
                        parseExpr "\\x y z -> x" `shouldReturn` "\\x y z -> x"
                it "let expression" $ do
                        parseExpr "let {id : A -> A; id = \\x -> x} in id a" `shouldReturn` "let {id : A -> A; id where {-> \\x -> x}} in id a"
        describe "Parsing a file" $ do
                it "test02.plt" $ do
                        pending

-- parseProg "test02.plt" `shouldReturn` ";data Bool where { True : Bool; False : Bool }"

parseExpr :: T.Text -> IO String
parseExpr inp = do
        exp <- runReaderT (parsePartial inp exprParser) =<< initUniq
        return $ show (pretty exp)

parseProg :: FilePath -> IO String
parseProg fn = do
        ast <- runReaderT (parseFile ("test/testcases/" ++ fn)) =<< initSession
        return $ show (pretty ast)