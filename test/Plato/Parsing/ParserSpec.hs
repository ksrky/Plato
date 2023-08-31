module Plato.Parsing.ParserSpec where

import Control.Monad.Reader
import Data.Text qualified as T
import Prettyprinter
import Test.Hspec

import Plato.Common.Uniq
import Plato.Driver.Monad
import Plato.Parsing

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
                it "01.pla" $ do
                        test_file "01.pla" `shouldReturn` ["data Bool where {True : Bool; False : Bool}"]
                it "02.pla" $ do
                        test_file "02.pla" `shouldReturn` ["id : {a} a -> a", "id where {-> \\x -> x}"]
                it "03.pla" $ do
                        test_file "03.pla" `shouldReturn` ["data Nat where {Zero : Nat; Succ : Nat -> Nat}"]
                it "04.pla" $ do
                        test_file "04.pla" `shouldReturn` ["g : {a b} (a -> b) -> a -> b", "g where {f x -> f x}"]
                it "05.pla" $ do
                        test_file "05.pla"
                                `shouldReturn` [ "data Bool where {True : Bool; False : Bool}"
                                               , "not : Bool -> Bool"
                                               , "not where {b -> case b of {True -> False; False -> True}}"
                                               ]
                it "08.pla" $ do
                        test_file "08.pla"
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
                it "09.pla" $ do
                        test_file "09.pla"
                                `shouldReturn` [ "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "infixr 5 ::"
                                               , "++ : {a} List a -> List a -> List a"
                                               , "++ where {Nil m -> m}"
                                               , "++ where {((x :: xs)) m -> (x :: (xs ++ m))}"
                                               , "infixr 5 ++"
                                               ]
                it "10.pla" $ do
                        test_file "10.pla"
                                `shouldReturn` [ "infixr 5 ::"
                                               , "data List a where {Nil : List a; :: : a -> List a -> List a}"
                                               , "reverse : {a} List a -> List a"
                                               , "reverse where {l -> let {rev : {a} List a -> List a -> List a; rev where {Nil a -> a}; rev where {((x :: xs)) a -> rev xs ((x :: a))}} in rev l Nil}"
                                               ]

test_expr :: T.Text -> IO String
test_expr inp = do
        exp <- runReaderT (parseExpr inp) =<< initUniq
        return $ show (pretty exp)

test_file :: FilePath -> IO [String]
test_file fn = do
        ast <- runReaderT (parseFile ("test/testcases/" ++ fn)) =<< initSession
        return $ map (show . pretty) ast