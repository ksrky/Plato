{-# LANGUAGE LambdaCase #-}

module Plato.Test.Core.KindInfer where

import Plato.Common.Error
import Plato.Common.Location
import Plato.Common.Monad
import Plato.Syntax.Typing
import Plato.Transl.PsToTyp qualified as T
import Plato.Transl.SrcToPs qualified as P
import Plato.Typing.KindInfer

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Plato.Syntax.Parsing qualified as P
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
        ,
                ( "data Monad m = Monad ({a} a -> m a) ({a b} m a -> (a -> m b) -> m b)"
                , (`shouldReturn` [ArrK (ArrK StarK StarK) StarK])
                )
                {-,
                        ( "data T g f x = T1 (f x) | T2 (g f)"
                        , (`shouldReturn` [(StarK `ArrK` StarK `ArrK` StarK) `ArrK` (StarK `ArrK` StarK) `ArrK` StarK `ArrK` StarK])
                        )-}
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