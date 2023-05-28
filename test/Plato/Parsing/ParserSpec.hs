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
                        test_expr "\\x -> x" `shouldReturn` "\\x -> x"
                it "function application" $ do
                        test_expr "f x y" `shouldReturn` "f x y"
                it "multiple lambda abstraction" $ do
                        test_expr "\\x y z -> x" `shouldReturn` "\\x y z -> x"
                it "let expression" $ do
                        test_expr "let {id : A -> A; id = \\x -> x} in id a" `shouldReturn` "let {id : A -> A; id where {-> \\x -> x}} in id a"
        describe "Parsing a file" $ do
                it "test01.plt" $ do
                        test_file "test01.plt" `shouldReturn` ["data Bool where {True : Bool; False : Bool}"]
                it "test02.plt" $ do
                        test_file "test02.plt" `shouldReturn` ["id : {a} a -> a", "id where {-> \\x -> x}"]
                it "test03.plt" $ do
                        test_file "test03.plt" `shouldReturn` ["data Nat where {Zero : Nat; Succ : Nat -> Nat}"]
                it "test04.plt" $ do
                        test_file "test04.plt" `shouldReturn` ["g : {a b} (a -> b) -> a -> b", "g where {f x -> f x}"]
                it "test05.plt" $ do
                        test_file "test05.plt"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "test08.plt" $ do
                        test_file "test08.plt"
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
                it "test00.plt" $ do
                        test_file "test09.plt"
                                `shouldReturn` [ "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "infixr 5 ::"
                                               , "++ : {a} List a -> List a -> List a"
                                               , "++ where {Nil m -> m}"
                                               , "++ where {(:: x xs) m -> (x :: (xs ++ m))}"
                                               , "infixr 5 ++"
                                               ]
                it "test10.plt" $ do
                        test_file "test10.plt"
                                `shouldReturn` [ "infixr 5 ::"
                                               , "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "reverse : {a} List a -> List a"
                                               , "reverse where {l -> let {rev : {a} List a -> List a -> List a; rev where {Nil a -> a}; rev where {(:: x xs) a -> rev xs\n((x :: a))}} in rev l Nil}"
                                               ]

test_expr :: T.Text -> IO String
test_expr inp = do
        exp <- runReaderT (parsePartial inp exprParser) =<< initUniq
        return $ show (pretty exp)

test_file :: FilePath -> IO [String]
test_file fn = do
        ast <- runReaderT (parseFile ("test/testcases/" ++ fn)) =<< initSession
        return $ map (show . pretty) ast