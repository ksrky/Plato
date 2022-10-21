module Plato.Test.TypTransl where

import Plato.Common.Error
import Plato.Syntax.Typing
import Plato.Test.Utils
import Plato.Translation.PsToTyp
import Plato.Translation.SrcToPs

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.Environment
import Test.Hspec

testcases :: [(String, IO Decls -> Expectation)]
testcases =
        [ ("01_bool.plt", undefined)
        ]

test :: (MonadThrow m, MonadIO m) => (String, m Expr -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ readFile src
                par_res <- src2ps (T.pack inp)
                undefined