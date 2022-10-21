{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Plato.Test.Typing.DeclTransl where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Parsing.Resolver
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Translation.PsToTyp

import Control.Exception.Safe
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO ([FuncDecl], [Located Decl]) -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                (fst -> [FD (VN "id") (NL (AbsE (VN "x") Nothing (VE "x"))) (NL (AllT [TV "a"] (NL (ArrT (VT "a") (VT "a")))))]) -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m ([FuncDecl], [Located Decl]) -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) declsParser)
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                transDecls ps'
