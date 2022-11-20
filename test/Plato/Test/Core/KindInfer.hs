{-# LANGUAGE LambdaCase #-}

module Plato.Test.Core.KindInfer where

import Plato.Syntax.Typing
import qualified Plato.Transl.PsToTyp as T
import qualified Plato.Transl.SrcToPs as P
import Plato.Types.Error
import Plato.Types.Location
import Plato.Types.Monad
import Plato.Typing.KindInfer

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Plato.Syntax.Parsing as P
import Test.Hspec

testcases :: [(String, IO [Kind] -> Expectation)]
testcases =
        [
                ( "data Bool = True | False"
                , (`shouldReturn` [StarK])
                )
        ,
                ( "data Maybe a = Nothing | Just a"
                , (`shouldReturn` [ArrK StarK StarK])
                )
        ,
                ( "data Either a b = Left a | Right b"
                , (`shouldReturn` [ArrK StarK (ArrK StarK StarK)])
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [Kind] -> Expectation) -> SpecWith ()
test (inp, iscorrect) =
        it inp $
                iscorrect $
                        ( returnPlato $ do
                                (fixenv, ps) <- P.src2ps (T.pack inp)
                                ps' <- P.psCanon [] fixenv ps
                                (tydecs, _, _) <- execWriterT $ T.transTopDecl (P.getModuleName ps') (head $ P.ps_topDecls ps')
                                forM tydecs $ \case
                                        (L _ (TypeD _ ty)) -> do
                                                (_, kn) <- inferKind M.empty ty
                                                return kn
                                        _ -> unreachable ""
                        )
                                initPInfo
                                initPState