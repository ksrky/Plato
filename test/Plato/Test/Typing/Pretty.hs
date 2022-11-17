module Plato.Test.Typing.Pretty where

import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Types.Monad

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test01.plt", (`shouldReturn` "Main\nBool = μBool. <True | False>\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\n"))
        , ("test02.plt", (`shouldReturn` "Main\nid = \\a. \\x:a. x : {a} a -> a\n"))
        , ("test03.plt", (`shouldReturn` "Main\nNat = μNat. <Zero | Succ Nat>\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\n"))
        , ("test04.plt", (`shouldReturn` "Main\ng = \\a b. \\f:a -> b. \\x:a. f x : {a b} (a -> b) -> a -> b\n"))
        , ("test05.plt", (`shouldReturn` "Main\nBool = μBool. <True | False>\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\nnot = \\b:Bool. case b : Bool of {\n    True  -> False\n    False  -> True\n} : Bool -> Bool\n"))
        , ("test06.plt", (`shouldReturn` "Main\nf = \\a. let {\n     g = \\b. \\x:b. x : {b} b -> b\n     h = \\c. \\y:c. y : {c} c -> c \n} in \\?x:a. (g a) ?x : {a} a -> a\n"))
        , ("test07.plt", (`shouldReturn` "Main\nBool = μBool. <True | False>\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\n(\\b:Bool. case b : Bool of {\n    True  -> False\n    False  -> True\n}) False"))
        , ("test08.plt", (`shouldReturn` "Main\nNat = μNat. <Zero | Succ Nat>\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\n+ = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> n\n    Succ m' -> Succ (+ m' n)\n} : Nat -> Nat -> Nat\n* = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> Zero\n    Succ m' -> + n (* m' n)\n} : Nat -> Nat -> Nat\n+ (* (Succ (Succ Zero)) (Succ Zero)) (Succ (Succ (Succ Zero)))"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) =
        it fname $
                iscorrect $ do
                        typ <-
                                ( returnPlato $ do
                                                let src = "test/testcases/" ++ fname
                                                inp <- liftIO $ T.readFile src
                                                (fixenv, ps) <- src2ps inp
                                                ps' <- psCanon [] fixenv ps
                                                ps2typ ps'
                                        )
                                        initPInfo
                                        initPState
                        return $ renderString $ layoutPretty defaultLayoutOptions (pretty typ)