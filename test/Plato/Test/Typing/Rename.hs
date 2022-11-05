module Plato.Test.Typing.Rename where

import Plato.Syntax.Typing as T
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Typing.Rename

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test08.plt", (`shouldReturn` ":l0 = { * = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> Zero\n    Succ m' -> (:l0.+) n ((:l0.*) m' n)\n} , + = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> n\n    Succ m' -> Succ ((:l0.+) m' n)\n} } : { * : Nat -> Nat -> Nat , + : Nat -> Nat -> Nat }"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                (_, _, ps) <- src2ps M.empty inp
                typ <- fst <$> ps2typ M.empty ps
                (fundec, _) <- renameFuncDs emptyRenameState (T.binds typ)
                return $ renderString $ layoutPretty defaultLayoutOptions (pretty fundec)