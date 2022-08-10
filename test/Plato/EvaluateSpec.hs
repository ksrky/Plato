{-# LANGUAGE OverloadedStrings #-}

module Plato.EvaluateSpec where

import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Eval
import Plato.Core.Syntax
import Plato.Debug.PrettyCore
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
                processFile [6]

iscorrect :: [Maybe String -> Expectation]
iscorrect =
        [ (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Nothing)
        , (`shouldBe` Just "False")
        , (`shouldBe` Just "3.0")
        , (`shouldBe` Nothing)
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
processCommand (Bind x bind) = state $ \ctx ->
        case getVarIndex x ctx of
                Just idx -> ((), cons (x, bind) (update idx (x, bind) ctx))
                Nothing -> ((), cons (x, bind) ctx)
processCommand (Eval t) = return ()

processEval :: Context -> [Command] -> Maybe Term
processEval ctx (Eval t : _) = Just $ eval ctx t
processEval ctx (cmd : cmds) = processEval ctx cmds
processEval ctx _ = Nothing

process :: String -> IO (Maybe String)
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                cmds <- execWriterT (transProgram ast) `evalStateT` initContext
                let ctx = mapM processCommand cmds `execState` initContext
                    res = processEval ctx cmds
                    ppres = pretty <$> res
                forM_ res print
                --putStrLn ""
                --print ctx
                return ppres
