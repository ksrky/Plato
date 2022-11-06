{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Test.Typing.DeclTransl where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Control.Exception.Safe
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO ([FuncD], [Located Decl]) -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                (fst -> [FuncD (VN "id") (ABSE "x" (VE "x")) (ALLT "a" (ArrT (VT "a") (VT "a")))]) -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m ([FuncD], [Located Decl]) -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine declsParser (T.pack inp))
                let OpTable = OpTable (parser_ust st)
                ps' <- mapM (resolve OpTable) ps
                transDecls ps'
