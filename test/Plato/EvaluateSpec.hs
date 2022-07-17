{-# LANGUAGE OverloadedStrings #-}

module Plato.EvaluateSpec where

import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Evaluate
import Plato.Core.Syntax
import Plato.Core.Translate
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.State
import Control.Monad.Writer.Lazy
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [0 .. 5]

iscorrect :: [Maybe Term -> Expectation]
iscorrect =
        [ (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Just (TmFloat 3.0))
        ]

processFile :: [Int] -> SpecWith ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "test/testcases/test" ++ show n ++ ".plt"
        it fname $ do
                contents <- readFile fname
                res <- process contents
                (iscorrect !! n) res
        processFile ns

processCommand :: Command -> State Context ()
processCommand (Import mod) = undefined
processCommand (Bind x bind) = modify ((x, bind) :)
processCommand (Eval t) = return ()

processEval :: Context -> [Command] -> Maybe Term
processEval ctx (Eval t : _) = Just $ eval ctx t
processEval ctx (cmd : cmds) = processEval ctx cmds
processEval ctx _ = Nothing

process :: String -> IO (Maybe Term)
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                let cmds = execWriterT (transProgram ast) `evalState` initContext
                    ctx = mapM processCommand cmds `execState` initContext
                    res = processEval ctx cmds
                forM_ res print
                return res
