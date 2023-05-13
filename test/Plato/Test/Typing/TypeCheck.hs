{-# LANGUAGE LambdaCase #-}

module Plato.Test.Typing.TypeCheck where

import Plato.Common.Monad
import Plato.Syntax.Typing
import Plato.Transl.PsToTyp
import Plato.Transl.SrcToPs

import Plato.Test.Typing.Utils
import Plato.Test.Utils

import Control.Exception.Safe
import Control.Monad.IO.Class
import Data.Text qualified as T
import Test.Hspec

testcases :: [(String, IO [FuncD] -> Expectation)]
testcases =
        [
                ( "id : {a} a -> a; id = \\x -> x"
                , ( `shouldSatisfyReturn`
                        \case
                                [FuncD (VA "id") ((TAbsE [TVA "a"] (AbsE (VA "x") (Just (SVT "a")) (VE "x")))) (AllT [(TV "a", _)] ((ArrT (VT "a") (VT "a"))))] -> True
                                _ -> False
                  )
                )
        ,
                ( "f : {a} a -> a; f = let { g : {b} b -> b; g y = y } in g"
                , ( `shouldSatisfyReturn`
                        \case
                                [FuncD (VA "f") (TAbsE [TVA "a"] (LetE [FuncD (VA "g") (TAbsE [TVA "b"] (AbsE (VA "y") (Just (SVT "b")) (VE "y"))) (AllT [(TV "b", _)] ((ArrT (VT "b") (VT "b"))))] (AbsE (VA "?x") (Just (SVT "a")) (AppE (TAppE (VE "g") [VarT (STV "a")]) (VE "?x"))))) (AllT [(TV "a", _)] ((ArrT (VT "a") (VT "a"))))] -> True
                                _ -> False
                  )
                )
        ]

test :: (MonadThrow m, MonadIO m) => (String, m [FuncD] -> Expectation) -> SpecWith ()
test (inp, iscorrect) =
        it inp $
                iscorrect $
                        ( returnPlato $ do
                                (fixenv, ps) <- src2ps (T.pack inp)
                                ps' <- psCanon [] fixenv ps
                                typ <- ps2typ ps'
                                return $ typ_binds typ
                        )
                                initPInfo
                                initPState