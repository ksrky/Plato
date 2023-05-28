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
                it "test01.plt" $ do
                        parseProg "test01.plt" `shouldReturn` ["data Bool where {True : Bool; False : Bool}"]
                it "test02.plt" $ do
                        parseProg "test02.plt" `shouldReturn` ["id : {a} a -> a", "id where {-> \\x -> x}"]
                it "test03.plt" $ do
                        parseProg "test03.plt" `shouldReturn` ["data Nat where {Zero : Nat; Succ : Nat -> Nat}"]
                it "test04.plt" $ do
                        parseProg "test04.plt" `shouldReturn` ["g : {a b} (a -> b) -> a -> b", "g where {f x -> f x}"]
                it "test05.plt" $ do
                        parseProg "test05.plt"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "test08.plt" $ do
                        parseProg "test08.plt"
                                `shouldReturn` [ "data Nat where {Zero : Nat; Succ : Nat -> Nat}"
                                               , "infixl 6 +"
                                               , "infixl 7 *"
                                               , "+ : Nat -> Nat -> Nat"
                                               , "+ where {Zero n -> n}"
                                               , "+ where {(Succ m) n -> Succ ((m + n))}"
                                               , "* : Nat -> Nat -> Nat"
                                               , "* where {Zero n -> Zero}"
                                               , "* where {(Succ m) n -> (n + (m * n))}"
                                               ]

parseExpr :: T.Text -> IO String
parseExpr inp = do
        exp <- runReaderT (parsePartial inp exprParser) =<< initUniq
        return $ show (pretty exp)

parseProg :: FilePath -> IO [String]
parseProg fn = do
        ast <- runReaderT (parseFile ("test/testcases/" ++ fn)) =<< initSession
        return $ map (show . pretty) ast