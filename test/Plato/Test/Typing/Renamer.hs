module Plato.Test.Typing.Renamer where

import Plato.Syntax.Typing as T
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Typing.Renamer

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Prettyprinter
import Prettyprinter.Render.String
import Test.Hspec

testcases :: [(String, IO String -> Expectation)]
testcases =
        [ ("test08.plt", (`shouldReturn` ":0 = { * = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> Zero\n    Succ m' -> (:0.+) n ((:0.*) m' n)\n} , + = \\m:Nat. \\n:Nat. case m : Nat of {\n    Zero  -> n\n    Succ m' -> Succ ((:0.+) m' n)\n} } : { * : Nat -> Nat -> Nat , + : Nat -> Nat -> Nat }"))
        ]

test :: (MonadThrow m, MonadIO m) => (String, m String -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ps <- src2ps inp
                typ <- fst <$> ps2typ M.empty ps
                (fundec, _) <- renameFuncDs emptyRenameState (T.binds typ)
                return $ renderString $ layoutPretty defaultLayoutOptions (pretty fundec)