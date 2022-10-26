{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.TypeCheck where

import Plato.Common.Error
import Plato.Common.Name
import Plato.Common.SrcLoc
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

testcases :: [(String, IO [FuncDecl] -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                [FD (VN "id") (NL (TAbsE [NL (Name TyvarName "a")] (NL (AbsE (VN "x") (Just (VarT (STV "a"))) (VE "x"))))) (NL (AllT [(TV "a", _)] (NL (ArrT (VT "a") (VT "a")))))] -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [FuncDecl] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) declsParser)
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                (fundecs, _) <- transDecls ps'
                mapM (typeCheck M.empty) fundecs
