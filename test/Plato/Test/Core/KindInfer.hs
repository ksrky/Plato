{-# LANGUAGE LambdaCase #-}

module Plato.Test.Core.KindInfer where

import Plato.Common.Error
import Plato.Common.SrcLoc
import Plato.Parsing.FixResol
import Plato.Parsing.Monad
import Plato.Parsing.Parser
import Plato.Syntax.Core
import qualified Plato.Syntax.Typing as T
import qualified Plato.Transl.PsToTyp as T
import qualified Plato.Transl.SrcToPs as P
import qualified Plato.Transl.TypToCore as C
import Plato.Typing.KindInfer

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Text as T
import Test.Hspec

testcases :: [(String, IO [Kind] -> Expectation)]
testcases =
        [
                ( "data Bool = True | False"
                , (`shouldReturn` [KnStar])
                )
        ,
                ( "data Maybe a = Nothing | Just a"
                , (`shouldReturn` [KnArr KnStar KnStar])
                )
        ,
                ( "data Either a b = Left a | Right b"
                , (`shouldReturn` [KnArr KnStar (KnArr KnStar KnStar)])
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [Kind] -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (ps, st) <- eitherToMonadThrow (P.parseLine topdeclParser (T.pack inp))
                let opdict = opDict (parser_ust st)
                ps' <- resolve opdict ps
                (tydecs, _, _) <- execWriterT $ T.transTopDecl ps'
                forM tydecs $ \case
                        (L _ (T.TypeD _ ty)) -> do
                                (_, kn) <- inferKind emptyKnTable ty
                                C.transKind kn
                        _ -> unreachable ""