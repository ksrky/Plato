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
                                                [ NL (TypeD (TCN "Bool") (RecT (TCN "Bool") (SumT [(CN "True", []), (CN "False", [])])))
                                                        , NL (ConD (FuncD (CN "True") (AppE (FoldE (CT "Bool")) (TagE (CN "True") [] (Just (CT "Bool")))) (CT "Bool")))
                                                        , NL (ConD (FuncD (CN "False") (AppE (FoldE (CT "Bool")) (TagE (CN "False") [] (Just (CT "Bool")))) (CT "Bool")))
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
                                        { binds = [FuncD (VN "id") (TAbsE [TVN "a"] (AbsE (VN "x") (Just (VarT (STV "a"))) (VE "x"))) (AllT [(TV "a", _)] (ArrT (VT "a") (VT "a")))]
                                        } -> True
                                _ -> False
                  )
                )
        ,
                ( "test06.plt"
                , ( `shouldSatisfyReturn`
                        \case
                                Program
                                        { binds =
                                                [ FuncD
                                                                f
                                                                ( TAbsE
                                                                                [TVN "a"]
                                                                                ( LetE
                                                                                                [ FuncD (VN "g") (TAbsE [TVN "b"] (AbsE (VN "x") (Just (SVT "b")) (VE "x"))) (AllT [(TV "a", _)] (ArrT (VT "a") (VT "a")))
                                                                                                        , FuncD (VN "h") (TAbsE [TVN "c"] (AbsE (VN "y") (Just (SVT "c")) (VarE y))) (AllT [(TV "a", _)] (ArrT (VT "a") (VT "a")))
                                                                                                        ]
                                                                                                (AbsE (VN "x") (Just (SVT "a")) (AppE (TAppE (VE "g") [VT "a"]) (VE "x")))
                                                                                        )
                                                                        )
                                                                (AllT [(TV "a", _)] (ArrT (VT "a") (VT "a")))
                                                        ]
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