module Plato.Test.Core.Pretty where

import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Core.Pretty
import Plato.Syntax.Core
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Transl.TypToCore

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test01.plt", (`shouldReturn` "Bool = \\Bool:*. <True | False> : *\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\n:l0 = fix \\:l0:{}. {} : {}\n"))
        , ("test02.plt", (`shouldReturn` ":l0 = fix \\:l0:{ id : {a:*}. a -> a }. { id = \\a. \\x:a. x } : { id : {a:*}. a -> a }\n"))
        , ("test03.plt", (`shouldReturn` "Nat = \\Nat:*. <Zero | Succ Nat> : *\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\n:l0 = fix \\:l0:{}. {} : {}\n"))
        , ("test04.plt", (`shouldReturn` ":l0 = fix \\:l0:{ g : {a:*}. {b:*}. (a -> b) -> a -> b }. { g = \\a. \\b. \\f:a -> b. \\x:a. f x } : { g : {a:*}. {b:*}. (a -> b) -> a -> b }\n"))
        , ("test05.plt", (`shouldReturn` "Bool = \\Bool:*. <True | False> : *\nTrue = fold [Bool] True : Bool\nFalse = fold [Bool] False : Bool\n:l0 = fix \\:l0:{ not : Bool -> Bool }. { not = \\b:Bool. case unfold [Bool] b of {\n    True 0 -> False\n    False 0 -> True\n} } : { not : Bool -> Bool }\n"))
        , ("test06.plt", (`shouldReturn` ":l0 = fix \\:l0:{ f : {a:*}. a -> a }. { f = \\a. let :l1 = fix \\:l1:{ h : {a:*}. a -> a , g : {a:*}. a -> a }. { h = \\c. \\y:c. y , g = \\b. \\x:b. x } in \\x:a. (:l1.g a) x } : { f : {a:*}. a -> a }\n"))
        , ("test08.plt", (`shouldReturn` "Nat = \\Nat:*. <Zero | Succ Nat> : *\nZero = fold [Nat] Zero : Nat\nSucc = \\1:Nat. fold [Nat] (Succ 1) : Nat -> Nat\n:l0 = fix \\:l0:{ * : Nat -> Nat -> Nat , + : Nat -> Nat -> Nat }. { * = \\m:Nat. \\n:Nat. case unfold [Nat] m of {\n    Zero 0 -> Zero\n    Succ 1 -> (:l0.+) n ((:l0.*) 1 n)\n} , + = \\m:Nat. \\n:Nat. case unfold [Nat] m of {\n    Zero 0 -> n\n    Succ 1 -> Succ ((:l0.+) 1 n)\n} } : { * : Nat -> Nat -> Nat , + : Nat -> Nat -> Nat }\n(:l0.+) ((:l0.*) (Succ (Succ Zero)) (Succ Zero)) (Succ (Succ (Succ Zero)))\n"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                (_, _, ps) <- src2ps M.empty inp
                typ <- fst <$> ps2typ M.empty ps
                cmds <- snd <$> typ2core [] emptyContext typ
                return $ renderString $ layoutPretty defaultLayoutOptions (pprcmds emptyContext cmds)

pprcmds :: Context -> [Command] -> Doc ann
pprcmds _ [] = emptyDoc
pprcmds ctx (c@Import{} : cmds) = vsep [ppr ctx c, pprcmds ctx cmds]
pprcmds ctx (c@(Bind x bind) : cmds) = vsep [ppr ctx c, pprcmds (V.cons (x, bind) ctx) cmds]
pprcmds ctx (Eval t : cmds) = vsep [ppr ctx (unLoc t), pprcmds ctx cmds]
