module Plato.Test.Parsing.Parser where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Plato.Transl.SrcToPs
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
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ps <- src2ps inp
                return $ show $ pretty ps