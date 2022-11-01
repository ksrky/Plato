module Plato.Test.Parsing.Pretty where

import Plato.Transl.SrcToPs

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Prettyprinter
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test01.plt", (`shouldReturn` "Bool = True | False"))
        , ("test02.plt", (`shouldReturn` "id : {a} a -> a\nid = \\x -> x"))
        , ("test03.plt", (`shouldReturn` "Nat = Zero | Succ Nat"))
        , ("test04.plt", (`shouldReturn` "g : {a b} (a -> b) -> a -> b\ng f x = f x"))
        , ("test05.plt", (`shouldReturn` "Bool = True | False\nnot : Bool -> Bool\nnot = \\b -> case b of {\n    True -> False\n    False -> True\n}"))
        , ("test06.plt", (`shouldReturn` "f : {a} a -> a\nf = let {\n    g : {b} b -> b\n    g = \\x -> x\n    h : {c} c -> c\n    h = \\y -> y\n} in g"))
        , ("test07.plt", (`shouldReturn` "Bool = True | False\n(\\b -> case b of {\n    True -> False\n    False -> True\n}) False"))
        , ("test09.plt", (`shouldReturn` "List a = Nil | :: a List a\n<fixity decl>\n++ : {a} List a -> List a -> List a\n++ l m = case l of {\n    Nil -> m\n    :: x xs -> x :: xs ++ m\n}\n<fixity decl>\nT = T1 | T2\nT1 :: T2 :: Nil ++ T2 :: T1 :: Nil"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ps <- src2ps inp
                return $ show $ pretty ps