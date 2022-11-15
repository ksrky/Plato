{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.ExprTransl where

import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser

import Plato.Syntax.Typing

import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Types.Location

import Control.Exception.Safe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Plato.Types.Monad
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
                                typ <- transExpr (noLoc ps)
                                return typ
                        )
                                initPInfo
                                initPState