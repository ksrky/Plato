{-# LANGUAGE OverloadedStrings #-}

module Plato.TranslateSpec where

import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Syntax.Lexer
import Plato.Syntax.Parser
import Plato.Translation.AbstractToCore

import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile []

iscorrect :: [[Command] -> Expectation]
iscorrect =
        []

processFile :: [Int] -> SpecWith ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "test/testcases/test" ++ show n ++ ".plt"
        it fname $ do
                contents <- readFile fname
                res <- process contents
                (iscorrect !! n) res
        processFile ns

process :: String -> IO [Command]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                cmds <- execWriterT (transProgram ast) `evalStateT` initContext
                print cmds >> return cmds
