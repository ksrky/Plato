{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.Transl where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Test.Hspec

testcases :: [(String, IO Program -> Expectation)]
testcases =
        [
                ( "test01.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Program
                                        { decls =
                                                [ NL (TypeD (TCN "Bool") (NL (RecT (TCN "Bool") (NL (SumT [(CN "True", []), (CN "False", [])])))))
                                                        , NL (FuncD (FD (CN "True") (NL (AppE (NL (FoldE (CT "Bool"))) (NL (TagE (CN "True") [] (Just (ConT (TCN "Bool"))))))) (CT "Bool")))
                                                        , NL (FuncD (FD (CN "False") (NL (AppE (NL (FoldE (CT "Bool"))) (NL (TagE (CN "False") [] (Just (ConT (TCN "Bool"))))))) (CT "Bool")))
                                                        ]
                                        } -> True
                                _ -> False
                  )
                )
        ,
                ( "test02.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Program
                                        { binds = [FD (VN "id") (NL (TAbsE [NL (Name TyvarName "a")] (NL (AbsE (VN "x") (Just (VarT (STV "a"))) (VE "x"))))) (NL (AllT [(TV "a", _)] (NL (ArrT (VT "a") (VT "a")))))]
                                        } -> True
                                _ -> False
                  )
                )
        ,
                ( "test06.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Program
                                        { binds = []
                                        } -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m Program -> Expectation) -> SpecWith ()
test (fname, iscorrect) = it fname $
        iscorrect $ do
                let src = "test/testcases/" ++ fname
                inp <- liftIO $ T.readFile src
                ps <- src2ps inp
                fst <$> ps2typ M.empty ps