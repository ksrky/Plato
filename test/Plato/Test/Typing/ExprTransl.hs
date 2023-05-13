{-# LANGUAGE LambdaCase #-}

module Plato.Test.Typing.ExprTransl where

import Plato.Common.Location
import Plato.Common.Monad
import Plato.Common.Name
import Plato.Common.Name.Global
import Plato.Syntax.Parsing qualified as Parsing
import Plato.Syntax.Typing
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Plato.Test.Typing.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Test.Hspec

testcases :: [(String, IO Expr -> Expectation)]
testcases =
        [
                ( "y + z"
                , ( `shouldSatisfyReturn`
                        \case
                                AppE (AppE (VE "+") (VE "y")) (VE "z") -> True
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

glbNameEnv :: GlbNameEnv
glbNameEnv =
        M.fromList
                [ (varName "y", GlbName Local (varName "y") NoSpan)
                , (varName "z", GlbName Local (varName "z") NoSpan)
                , (varName "+", GlbName Local (varName "+") NoSpan)
                , (tyconName "T", GlbName Local (tyconName "T") NoSpan)
                , (conName "True", GlbName Local (conName "True") NoSpan)
                , (conName "False", GlbName Local (conName "False") NoSpan)
                ]

test :: (MonadThrow m, MonadFail m) => (String, m Expr -> Expectation) -> SpecWith ()
test (inp, iscorrect) =
        it inp $
                iscorrect $
                        ( returnPlato $ do
                                Parsing.Program _ _ [L _ (Parsing.Eval exp)] <- exp2ps $ T.pack inp
                                transExpr exp
                        )
                                initPInfo
                                initPState{plt_glbNameEnv = glbNameEnv}