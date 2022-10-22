{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.ExprTransl where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Parsing.Resolver
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Transl.PsToTyp

import Control.Exception.Safe
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO Expr -> Expectation)]
testcases =
        [
                ( "x + y"
                , ( `shouldSatisfyReturn`
                        \case
                                AppE (NL (AppE (VE "+") (VE "x"))) (VE "y") -> True
                                _ -> False
                  )
                )
        ,
                ( "\\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                AbsE (VN "x") Nothing (VE "x") -> True
                                _ -> False
                  )
                )
        ,
                ( "let { x : T; x = y } in x"
                , ( `shouldSatisfyReturn`
                        \case
                                LetE [FD (VN "x") (VE "y") (CT "T")] (VE "x") -> True
                                _ -> False
                  )
                )
        ,
                ( "case True of { True -> False; False -> True }"
                , ( `shouldSatisfyReturn`
                        \case
                                CaseE (VE "True") Nothing [(CP "True" [], CE "False"), (CP "False" [], CE "True")] -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m Expr -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) exprParser)
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                typ <- transExpr ps'
                return $ unLoc typ
