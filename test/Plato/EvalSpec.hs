{-# LANGUAGE OverloadedStrings #-}

module Plato.EvaluateSpec where

import Plato.Abstract.Lexer
import Plato.Abstract.Parser
import Plato.Common.Pretty
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Core.Syntax
import Plato.Translation.AbstractToIR
import Plato.Translation.IRToCore

import Control.Exception.Safe
import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFiles testcases

testcases :: [(String, String -> Expectation)]
testcases =
        [ ("01_bool.plt", (`shouldBe` "False"))
        , ("02_natural.plt", (`shouldBe` "(Succ (Succ (Succ (Succ Zero)))"))
        , ("03_forall.plt", (`shouldBe` "p"))
        , ("04_mutual.plt", (`shouldBe` "True"))
        , ("05_maybe.plt", (`shouldBe` "Just T1"))
        , ("06_list.plt", (`shouldBe` "Just T1"))
        ]

processFiles :: [(String, String -> Expectation)] -> SpecWith ()
processFiles [] = return ()
processFiles ((fname, iscorrect) : rest) = do
        let src = "test/testcases/" ++ fname
        it fname $ do
                contents <- readFile fname
                res <- process emptyContext contents
                iscorrect res
        processFiles rest

process :: (MonadThrow m, MonadFail m) => Context -> String -> m String
process ctx input = case runAlex input parse of
        Left msg -> throwString msg
        Right ast -> do
                inner <- abstract2ir ast
                cmds <- ir2core ctx inner
                let ctx' = foldl (flip cons) ctx (binds cmds)
                tyT <- typeof ctx' (body cmds)
                let res = eval ctx' (body cmds)
                return $ pretty (ctx', res)