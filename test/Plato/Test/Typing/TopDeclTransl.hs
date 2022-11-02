{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Typing.TopDeclTransl where

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
import Control.Monad.Writer
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [Decl] -> Expectation)]
testcases =
        [
                ( "data Bool = True | False"
                , ( `shouldSatisfyReturn`
                        \case
                                [ TypeD (TCN "Bool") (RecT (TCN "Bool") (SumT [(CN "True", []), (CN "False", [])]))
                                        , ConD (FuncD (CN "True") (AppE (FoldE (CT "Bool")) (TagE (CN "True") [] (SumT [(CN "True", []), (CN "False", [])]))) (CT "Bool"))
                                        , ConD (FuncD (CN "False") (AppE ((FoldE (CT "Bool"))) (TagE (CN "False") [] (SumT [(CN "True", []), (CN "False", [])]))) (CT "Bool"))
                                        ] -> True
                                _ -> False
                  )
                )
        ]

test :: MonadThrow m => (String, m [Decl] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (parseLine topdeclParser (T.pack inp))
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                (tydecs, fundecs, _) <- execWriterT $ transTopDecl ps'
                return $ map unLoc tydecs ++ map (ConD . unLoc) fundecs
