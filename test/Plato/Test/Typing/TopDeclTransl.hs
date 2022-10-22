{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.TopDeclTransl where

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
import Control.Monad.Writer
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [Decl] -> Expectation)]
testcases =
        [
                ( "data Bool = True | False"
                , ( `shouldSatisfyReturn`
                        \case
                                [ TypeD (TCN "Bool") (NL (SumT [(CN "True", []), (CN "False", [])]))
                                        , FuncD (FD (CN "True") (NL (TagE (CN "True") [] (Just (ConT (TCN "Bool"))))) (CT "Bool"))
                                        , FuncD (FD (CN "False") (NL (TagE (CN "False") [] (Just (ConT (TCN "Bool"))))) (CT "Bool"))
                                        ] -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m [Decl] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine (T.pack inp) topdeclParser)
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                (tydecs, fundecs) <- execWriterT $ transTopDecl ps'
                return $ map unLoc tydecs ++ map (FuncD . unLoc) fundecs
