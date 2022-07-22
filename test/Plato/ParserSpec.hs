{-# LANGUAGE OverloadedStrings #-}

module Plato.ParserSpec where

import Plato.Common.Info
import Plato.Debug.Parser
import Plato.Syntax.AST
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.Trans

import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [6]

iscorrect :: [[TopDecl] -> Expectation]
iscorrect =
        [ (`shouldBe` [Decl (FuncTyDecl dummyInfo "id" (AllType "x" (FunType dummyInfo (VarType dummyInfo "x") (VarType dummyInfo "x")))), Decl (FuncDecl dummyInfo "id" (LamExpr dummyInfo "a" (VarExpr dummyInfo "a" [])))])
        , (`shouldBe` [DataDecl dummyInfo "Bool" [] [("True", []), ("False", [])]])
        , (`shouldBe` [TypeDecl dummyInfo "Number" [] (ConType dummyInfo "Float")])
        , (`shouldBe` [Decl (FuncTyDecl dummyInfo "func" (ConType dummyInfo "Float")), Decl (FuncDecl dummyInfo "func" (LetExpr dummyInfo [FuncDecl dummyInfo "f" (FloatExpr 1.0)] (VarExpr dummyInfo "f" [])))])
        , (`shouldBe` [DataDecl dummyInfo "Bool" [] [("True", []), ("False", [])], Decl (FuncTyDecl dummyInfo "not" (FunType dummyInfo (ConType dummyInfo "Bool") (ConType dummyInfo "Bool"))), Decl (FuncDecl dummyInfo "not" (LamExpr dummyInfo "b" (CaseExpr dummyInfo (VarExpr dummyInfo "b" []) [(ConExpr dummyInfo "True" [], ConExpr dummyInfo "False" [], dummyInfo), (ConExpr dummyInfo "False" [], ConExpr dummyInfo "True" [], dummyInfo)]))), Decl (FuncTyDecl dummyInfo "main" (ConType dummyInfo "Bool")), Decl (FuncDecl dummyInfo "main" (VarExpr dummyInfo "not" [ConExpr dummyInfo "True" []]))])
        , (`shouldBe` [Decl (FuncTyDecl dummyInfo "id" (FunType dummyInfo (ConType dummyInfo "Float") (ConType dummyInfo "Float"))), Decl (FuncDecl dummyInfo "id" (LamExpr dummyInfo "a" (VarExpr dummyInfo "a" []))), Decl (FuncTyDecl dummyInfo "main" (ConType dummyInfo "Float")), Decl (FuncDecl dummyInfo "main" (VarExpr dummyInfo "id" [FloatExpr 3.0]))])
        , (`shouldBe` [DataDecl dummyInfo "Nat" [] [("Zero", []), ("Succ", [ConType dummyInfo "Nat"])], Decl (FuncTyDecl dummyInfo "plus" (FunType dummyInfo (ConType dummyInfo "Nat") (FunType dummyInfo (ConType dummyInfo "Nat") (ConType dummyInfo "Nat")))), Decl (FuncDecl dummyInfo "plus" (LamExpr dummyInfo "m" (LamExpr dummyInfo "n" (CaseExpr dummyInfo (VarExpr dummyInfo "m" []) [(ConExpr dummyInfo "Zero" [], VarExpr dummyInfo "n" [], dummyInfo), (ConExpr dummyInfo "Succ" [VarExpr dummyInfo "m'" []], ConExpr dummyInfo "Succ" [VarExpr dummyInfo "plus" [VarExpr dummyInfo "m'" [VarExpr dummyInfo "n" []]]], dummyInfo)])))), Decl (FuncTyDecl dummyInfo "main" (ConType dummyInfo "Nat")), Decl (FuncDecl dummyInfo "main" (VarExpr dummyInfo "plus" [ConExpr dummyInfo "Succ" [ConExpr dummyInfo "Succ" [ConExpr dummyInfo "Zero" []]], ConExpr dummyInfo "Succ" [ConExpr dummyInfo "Succ" [ConExpr dummyInfo "Succ" [ConExpr dummyInfo "Zero" []]]]]))])
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
