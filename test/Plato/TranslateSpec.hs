{-# LANGUAGE OverloadedStrings #-}

module Plato.TranslateSpec where

import Plato.Common.Name
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Core.Translate
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.State
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [0 .. 5]

iscorrect :: [[Command] -> Expectation]
iscorrect =
        [ ( `shouldBe`
                [Bind "id" (VarBind (TyAll "x" KnStar (TyArr (TyVar 0 3) (TyVar 0 3)))), Bind "id" (TmAbbBind (TmTAbs "x" KnStar (TmAbs "a" (TyVar 0 3) (TmVar 0 5))) (Just (TyAll "x" KnStar (TyArr (TyVar 0 3) (TyVar 0 3)))))]
          )
        , (`shouldBe` [Bind "Bool" (TyAbbBind (TyVariant [("True", []), ("False", [])]) Nothing)])
        , (`shouldBe` [Bind "Number" (TyAbbBind (TyVar 1 2) Nothing)])
        , (`shouldBe` [Bind "func" (VarBind (TyVar 1 2)), Bind "func" (TmAbbBind (TmLet "f" (TmFloat 1.0) (TmVar 0 4)) (Just (TyVar 1 2)))])
        , ( `shouldBe`
                [Bind "Bool" (TyAbbBind (TyVariant [("True", []), ("False", [])]) Nothing), Bind "not" (VarBind (TyArr (TyVar 0 5) (TyVar 0 5))), Bind "not" (TmAbbBind (TmAbs "b" (TyVar 0 5) (TmCase (TmVar 0 7) [(TmVar 4 7, TmVar 3 7), (TmVar 3 7, TmVar 4 7)])) (Just (TyArr (TyVar 0 5) (TyVar 0 5))))]
          )
        , (`shouldBe` [Bind "id" (VarBind (TyArr (TyVar 1 2) (TyVar 1 2))), Bind "main" (VarBind (TyVar 2 3)), Bind "id" (TmAbbBind (TmAbs "a" (TyVar 1 2) (TmVar 0 5)) (Just (TyArr (TyVar 1 2) (TyVar 1 2)))), Eval (TmApp (TmVar 0 6) (TmFloat 3.0))])
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

process :: String -> IO [Command]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                let cmds = evalState (transProgram ast) initContext
                print cmds >> return cmds
