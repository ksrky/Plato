{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Plato.Test.Parsing.FixResol where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Parsing.FixResol
import Plato.Parsing.Fixity
import Plato.Parsing.Parser
import Plato.Syntax.Parsing
import Plato.Transl.SrcToPs

import Plato.Test.Utils

import Control.Exception.Safe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

-- See https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html to know aboutn pattern synonims
pattern X :: Located Expr
pattern X <- L _ (VarE (L _ (Name VarName "x")))

pattern Y :: Located Expr
pattern Y <- L _ (VarE (L _ (Name VarName "y")))

pattern Z :: Located Expr
pattern Z <- L _ (VarE (L _ (Name VarName "z")))

pattern Plus :: Located Name
pattern Plus <- (L _ (Name VarName "+"))

pattern Times :: Located Name
pattern Times <- L _ (Name VarName "*")

pattern Append :: Located Name
pattern Append <- L _ (Name VarName "++")

pattern GT :: Located Name
pattern GT <- L _ (Name VarName ">")

testcases :: [(String, IO Expr -> Expectation)]
testcases =
        [ ("x + y", (`shouldSatisfyReturn` \case OpE X Plus Y -> True; _ -> False))
        , ("x + y * z", (`shouldSatisfyReturn` \case OpE X Plus (L _ (OpE Y Times Z)) -> True; _ -> False))
        , ("x ++ y ++ z", (`shouldSatisfyReturn` \case OpE (L _ (OpE X Append Y)) Append Z -> True; _ -> False))
        , ("x > y > z", (`shouldThrow` anyException))
        ]

opdict :: OpDict
opdict =
        M.fromList
                [ (varName "+", Op (L NoSpan (Name VarName "+")) 6 Leftfix)
                , (varName "*", Op (L NoSpan (Name VarName "*")) 7 Leftfix)
                , (varName "++", Op (L NoSpan (Name VarName "++")) 5 Leftfix)
                , (varName ">", Op (L NoSpan (Name VarName ">")) 4 Nonfix)
                ]

test :: MonadThrow m => (String, m Expr -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (res, _) <- eitherToMonadThrow (parseLine exprParser (T.pack inp))
                unLoc <$> resolve opdict res