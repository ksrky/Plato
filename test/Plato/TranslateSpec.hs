{-# LANGUAGE OverloadedStrings #-}

module Plato.TranslateSpec where

import Plato.Common.Name
import Plato.Common.Position
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.Core.Translate
import Plato.Syntax.AST
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.State
import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [0 .. 3]

iscorrect :: [[(Name, Term)] -> Expectation]
iscorrect =
        [ (`shouldBe` [("id", TmTAbs "x" KnStar (TmAbs "a" (TyVar 0 1) (TmVar 0 3)))])
        , (`shouldBe` [])
        , (`shouldBe` [])
        , (`shouldBe` [])
        ]

-- Decl (FuncTyDecl "double" (TyApp "Int" (TyApp "Int" "Int") (Pos {line=3, col=1}))), FuncDecl "double" (LamExpr "x")
processFile :: [Int] -> SpecWith ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "test/testcases/test" ++ show n ++ ".plt"
        it fname $ do
                contents <- readFile fname
                res <- process contents
                (iscorrect !! n) res
        processFile ns

process :: String -> IO [(Name, Term)]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right ast -> do
                let res = evalState (transProgram ast) emptyContext
                print res >> return res
