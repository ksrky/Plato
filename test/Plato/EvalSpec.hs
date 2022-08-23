{-# LANGUAGE OverloadedStrings #-}

module Plato.EvalSpec where

import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Command
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Translation.AbsToInt
import Plato.Translation.IntToCore
import Plato.Translation.SrcToAbs

import Control.Exception.Safe
import Control.Monad.State
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Eval" $ do
                processFiles testcases

testcases :: [(String, String -> Expectation)]
testcases =
        [ ("01_bool.plt", (`shouldBe` "False"))
        , ("02_natural.plt", (`shouldBe` "Succ (Succ (Succ Zero))"))
        , ("03_forall.plt", (`shouldBe` "p"))
        , ("04_mutual.plt", (`shouldBe` "True"))
        , ("05_maybe.plt", (`shouldBe` "Just T1"))
        , ("06_list.plt", (`shouldBe` "Just T1"))
        , ("07_infix.plt", (`shouldBe` "Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))"))
        ]

processFiles :: [(String, String -> Expectation)] -> SpecWith ()
processFiles [] = return ()
processFiles ((fname, iscorrect) : rest) = do
        let src = "test/testcases/" ++ fname
        it src $ do
                contents <- readFile src
                res <- process emptyContext contents
                iscorrect res
        processFiles rest

process :: (MonadThrow m, MonadFail m) => Context -> String -> m String
process ctx input = do
        ast <- src2abs input
        ir <- abs2int ast
        cmds <- int2core ctx ir
        ctx' <- (`execStateT` ctx) $
                forM_ (binds cmds) $ \(fi, (x, bind)) -> do
                        ctx <- get
                        checkBinding fi ctx bind
                        put $ cons (x, bind) ctx
        tyT <- typeof ctx' (body cmds)
        let res = eval ctx' (body cmds)
        return $ pretty (ctx', res)
