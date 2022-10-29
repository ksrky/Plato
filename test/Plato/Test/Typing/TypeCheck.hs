{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.TypeCheck where

import Plato.Common.Error
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Typing
import Plato.Test.Typing.Utils
import Plato.Test.Utils
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs
import Plato.Typing.TypeCheck

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [FuncD] -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                [FuncD (VN "id") ((TAbsE [TVN "a"] (AbsE (VN "x") (Just (SVT "a")) (VE "x")))) (AllT [(TV "a", _)] ((ArrT (VT "a") (VT "a"))))] -> True
                                _ -> False
                  )
                )
                {-,
                        ( "f : {a} a -> a; f = let { g : {b} b -> b; g y = y } in g"
                        , ( `shouldSatisfyReturn`
                                \case
                                        [FuncD (VN "f") (TAbsE [TVN "a"] (LetE [FuncD (VN "g") (TAbsE [TVN "b"] (AbsE (VN "y") (Just (SVT "b")) (VE "y"))) (AllT [(TV "a", _)] ((ArrT (VT "a") (VT "a"))))] (AbsE (VN "x") (Just (SVT "a")) (VE "x")))) (AllT [(TV "a", _)] ((ArrT (VT "a") (VT "a"))))] -> True
                                        _ -> False
                          )
                        )-}
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [FuncD] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) declsParser)
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                (fundecs, _) <- transDecls ps'
                mapM (typeCheck M.empty) fundecs
