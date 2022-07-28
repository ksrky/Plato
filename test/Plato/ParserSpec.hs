{-# LANGUAGE OverloadedStrings #-}

module Plato.ParserSpec where

import Plato.Common.Info
import Plato.Syntax.Abstract
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.Trans

import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile []

iscorrect :: [[TopDecl] -> Expectation]
iscorrect =
        [ (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        ]

processFile :: [Int] -> SpecWith ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "test/testcases/test" ++ show n ++ ".plt"
        it fname $ do
                contents <- readFile fname
                res <- process contents
                let res' = map reduceTopDecl res
                (iscorrect !! n) res'
        processFile ns

process :: String -> IO [TopDecl]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right res -> do
                let res' = map reduceTopDecl res
                print res' >> return res
