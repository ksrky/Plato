{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.Transl where

import Plato.Common.Error
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Test.Hspec

testcases :: [(String, IO Decls -> Expectation)]
testcases =
        [
                ( "test01.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Decls
                                        { imports = []
                                        , decls =
                                                [ NL (TypeD (TCN "Bool") (NL (SumT [(CN "True", []), (CN "False", [])])))
                                                        , NL (FuncD (FD (CN "True") (NL (TagE (CN "True") [] (Just (ConT (TCN "Bool"))))) (CT "Bool")))
                                                        , NL (FuncD (FD (CN "False") (NL (TagE (CN "False") [] (Just (ConT (TCN "Bool"))))) (CT "Bool")))
                                                        ]
                                        , body = []
                                        } -> True
                                _ -> False
                  )
                )
        ,
                ( "test02.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Decls
                                        { imports = []
                                        , decls = []
                                        , body = [FD (VN "id") (NL (AbsE (VN "x") Nothing (VE "x"))) (NL (AllT [TV "a"] (NL (ArrT (VT "a") (VT "a")))))]
                                        } -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m Decls -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ps <- src2ps inp
                ps2typ ps