{-# LANGUAGE LambdaCase #-}

module Plato.Test.Typing.ExprTransl where

import Plato.Syntax.Typing
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Types.Location
import Plato.Types.Monad

import Plato.Test.Typing.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO Expr -> Expectation)]
testcases =
        [
                ( "x + y"
                , ( `shouldSatisfyReturn`
                        \case
                                AppE (AppE (VE "+") (VE "x")) (VE "y") -> True
                                _ -> False
                  )
                )
        ,
                ( "\\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                AbsE (VA "x") Nothing (VE "x") -> True
                                _ -> False
                  )
                )
        ,
                ( "let { x : T; x = y } in x"
                , ( `shouldSatisfyReturn`
                        \case
                                LetE [FuncD (VA "x") (VE "y") (CT "T")] (VE "x") -> True
                                _ -> False
                  )
                )
        ,
                ( "case True of { True -> False; False -> True }"
                , ( `shouldSatisfyReturn`
                        \case
                                CaseE (CE "True") Nothing [(CP "True" [], CE "False"), (CP "False" [], CE "True")] -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m Expr -> Expectation) -> SpecWith ()
test (inp, iscorrect) =
        it inp $
                iscorrect $
                        ( returnPlato $ do
                                ps <- exp2ps $ T.pack inp
                                transExpr (noLoc ps)
                        )
                                initPInfo
                                initPState