{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Plato.Test.Core.TermTransl where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Core.Context
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Core as C
import qualified Plato.Syntax.Core as C
import qualified Plato.Syntax.Typing as T
import qualified Plato.Transl.PsToTyp as T
import qualified Plato.Transl.SrcToPs as P
import qualified Plato.Transl.TypToCore as C
import Plato.Typing.KindInfer

import Plato.Test.Core.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import Control.Monad.State
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [(C.Term, C.Ty)] -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                [(TmTAbs (TVN "a") (TmAbs (VN "x") (TyVar 0 1) (TmVar 0 2)), _)] -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [(C.Term, C.Ty)] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (P.parseLine (T.pack inp) declsParser)
                let opdict = opDict (parser_ust st)
                ps' <- mapM (resolve opdict) ps
                (fundecs, _) <- T.transDecls ps'
                fundecs' <- T.processDecls [] fundecs
                (binds, _) <- mapM (StateT . C.transDecl . T.FuncD) fundecs' `runStateT` (emptyContext, emptyKnTable)
                forM binds $ \(_, b) -> case b of
                        C.TmAbbBind t (Just tyT) -> return (unLoc t, unLoc tyT)
                        _ -> unreachable ""