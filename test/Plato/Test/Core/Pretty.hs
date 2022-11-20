module Plato.Test.Core.Pretty where

import Plato.Core.Context
import Plato.Core.Pretty
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore
import Plato.Types.Monad

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test01.plt", (`shouldReturn` "Main\nBool = \\Bool:*. <True | False> : *\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\nMain = fix \\Main:{}. {} : {}\n"))
        , ("test02.plt", (`shouldReturn` "Main\nMain = fix \\Main:{ id : {a:*} a -> a }. { id = \\a. \\x:a. x } : { id : {a:*} a -> a }\n"))
        , ("test03.plt", (`shouldReturn` "Main\nNat = \\Nat:*. <Zero | Succ Nat> : *\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\nMain = fix \\Main:{}. {} : {}\n"))
        , ("test04.plt", (`shouldReturn` "Main\nMain = fix \\Main:{ g : {a:*} {b:*} (a -> b) -> a -> b }. { g = \\a. \\b. \\f:a -> b. \\x:a. f x } : { g : {a:*} {b:*} (a -> b) -> a -> b }\n"))
        , ("test05.plt", (`shouldReturn` "Main\nBool = \\Bool:*. <True | False> : *\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\nMain = fix \\Main:{ not : Bool -> Bool }. { not = \\b:Bool. case unfold [Bool] b of {\n    True 0 -> False\n    False 0 -> True\n} } : { not : Bool -> Bool }\n"))
        , ("test06.plt", (`shouldReturn` "Main\nMain = fix \\Main:{ f : {a:*} a -> a }. { f = \\a. let :l1 = fix \\:l1:{ g : {b:*} b -> b, h : {c:*} c -> c }. { g = \\b. \\x:b. x, h = \\c. \\y:c. y } in \\?x:a. (:l1.g a) ?x } : { f : {a:*} a -> a }\n"))
        , ("test08.plt", (`shouldReturn` "Main\nNat = \\Nat:*. <Zero | Succ Nat> : *\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\nMain = fix \\Main:{ + : Nat -> Nat -> Nat, * : Nat -> Nat -> Nat }. { + = \\m:Nat. \\n:Nat. case unfold [Nat] m of {\n    Zero 0 -> n\n    Succ 1 -> Succ ((Main.+) 1 n)\n}, * = \\m:Nat. \\n:Nat. case unfold [Nat] m of {\n    Zero 0 -> Zero\n    Succ 1 -> (Main.+) n ((Main.*) 1 n)\n} } : { + : Nat -> Nat -> Nat, * : Nat -> Nat -> Nat }\n(Main.+) ((Main.*) (Succ (Succ Zero)) (Succ Zero)) (Succ (Succ (Succ Zero)))"))
        , ("test18.plt", (`shouldReturn` "Main\nBool = \\Bool:*. <True | False> : *\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\nanything = {a:*} a\nMain = fix \\Main:{ not : Bool -> Bool }. { not = \\b:Bool. case unfold [Bool] b of {\n    True 0 -> anything Bool\n    False 0 -> anything Bool\n} } : { not : Bool -> Bool }\n(Main.not) True"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ( returnPlato $ do
                                (fixenv, ps) <- src2ps inp
                                ps' <- psCanon [] fixenv ps
                                typ <- ps2typ ps'
                                mod <- typ2core typ
                                return $ renderString $ layoutPretty defaultLayoutOptions (ppr emptyContext mod)
                        )
                        initPInfo
                        initPState