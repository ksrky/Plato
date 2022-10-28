module Plato.Test.Core.KindInfer where

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

import Plato.Test.Utils

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [Kind] -> Expectation)]
testcases =
        [
                ( "data Bool = True | False"
                , (`shouldReturn` [C.KnStar])
                )
        ,
                ( "data Maybe a = Nothing | Just a"
                , (`shouldReturn` [C.KnArr C.KnStar C.KnStar])
                )
        ,
                ( "data Either a b = Left a | Right b"
                , (`shouldReturn` [C.KnArr C.KnStar (C.KnArr C.KnStar C.KnStar)])
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [Kind] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (P.parseLine (T.pack inp) topdeclParser)
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                (tydecs, fundecs, _) <- execWriterT $ T.transTopDecl ps'
                forM tydecs $ \(L _ (T.TypeD name ty)) -> do
                        (ty', kn) <- inferKind emptyKnTable ty
                        C.transKind kn