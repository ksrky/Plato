{-# LANGUAGE OverloadedStrings #-}

module Plato.ParserSpec where

import Plato.Common.Position
import Plato.Syntax.AST
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.Trans

import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [0 .. 5]

iscorrect :: [[TopDecl] -> Expectation]
iscorrect =
        [ ( `shouldBe`
                [ Decl (FuncTyDecl "id" (AllType "x" (FunType (VarType "x" Pos{line = 3, col = 16}) (VarType "x" Pos{line = 3, col = 21}) Pos{line = 3, col = 18})) Pos{line = 3, col = 1})
                , Decl (FuncDecl "id" (LamExpr "a" (VarExpr "a" [] Pos{line = 4, col = 12}) Pos{line = 4, col = 6}) Pos{line = 4, col = 1})
                ]
          )
        , (`shouldBe` [DataDecl "Bool" [] [("True", []), ("False", [])] (Pos{line = 3, col = 1})])
        , (`shouldBe` [TypeDecl "Number" [] (ConType "Float" (Pos{line = 3, col = 15})) (Pos{line = 3, col = 1})])
        , ( `shouldBe`
                [ Decl (FuncTyDecl "func" (ConType "Float" Pos{line = 3, col = 8}) Pos{line = 3, col = 1})
                , Decl (FuncDecl "func" (LetExpr [FuncDecl "f" (FloatExpr 1.0) Pos{line = 4, col = 14}] (VarExpr "f" [] Pos{line = 4, col = 24}) Pos{line = 4, col = 8}) Pos{line = 4, col = 1})
                ]
          )
        , ( `shouldBe`
                [ DataDecl "Bool" [] [("True", []), ("False", [])] Pos{line = 3, col = 1}
                , Decl (FuncTyDecl "not" (FunType (ConType "Bool" Pos{line = 5, col = 7}) (ConType "Bool" Pos{line = 5, col = 15}) Pos{line = 5, col = 12}) Pos{line = 5, col = 1})
                , Decl (FuncDecl "not" (LamExpr "b" (CaseExpr (VarExpr "b" [] Pos{line = 6, col = 18}) [(ConExpr "True" [] Pos{line = 6, col = 25}, ConExpr "False" [] Pos{line = 6, col = 33}, Pos{line = 6, col = 30}), (ConExpr "False" [] Pos{line = 6, col = 40}, ConExpr "True" [] Pos{line = 6, col = 49}, Pos{line = 6, col = 46})] Pos{line = 6, col = 13}) Pos{line = 6, col = 7}) Pos{line = 6, col = 1})
                ]
          )
        , (`shouldBe` [Decl (FuncTyDecl "id" (FunType (ConType "Float" (Pos 3 6)) (ConType "Float" (Pos 3 15)) (Pos 3 12)) (Pos 3 1)), Decl (FuncDecl "id" (LamExpr "a" (VarExpr "a" [] (Pos 4 12)) (Pos 4 6)) (Pos 4 1)), Decl (FuncTyDecl "main" (ConType "Float" (Pos 6 8)) (Pos 6 1)), Decl (FuncDecl "main" (VarExpr "id" [FloatExpr 3.0] (Pos 7 8)) (Pos 7 1))])
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

process :: String -> IO [TopDecl]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right res -> print res >> return res
