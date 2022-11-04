module Plato.Test.Parsing.Pretty where

import Plato.Transl.SrcToPs

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
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
        , ("test13.plt", (`shouldReturn` "List = Nil | :: a List a\nreverse : {a} List a -> List a\nreverse l = let {\n    rev : {a} List a -> List a -> List a\n    rev l' a = case l' of {\n        Nil -> a\n        :: x xs -> rev xs (x :: a)\n    }\n} in rev l Nil"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                (_, _, ps) <- src2ps M.empty inp
                return $ show $ pretty ps