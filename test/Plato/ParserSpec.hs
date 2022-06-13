module Plato.ParserSpec where

import Plato.Syntax.AST
import Plato.Syntax.Lexer
import Plato.Syntax.Parser

import Control.Monad.Trans

import System.Environment
import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Parser" $ do
                processFile [0 .. 2]

iscorrect :: [[TopDecl] -> Expectation]
iscorrect =
        [ (`shouldBe` [Decl (FuncTyDecl "id" (TyFun (TyCon "Int" (Pos{line = 3, col = 6})) (TyCon "Int" (Pos{line = 3, col = 13})) (Pos{line = 3, col = 10})) (Pos{line = 3, col = 1})), Decl (FuncDecl "id" (LamExpr "x" (CallExpr "x" [] (Pos{line = 4, col = 12})) (Pos{line = 4, col = 6})) (Pos{line = 4, col = 1}))])
        , (`shouldBe` [DataDecl "Bool" [] [("True", []), ("False", [])] (Pos{line = 3, col = 1})])
        , (`shouldBe` [TypeDecl "Number" [] (TyCon "Int" (Pos{line = 3, col = 15})) (Pos{line = 3, col = 1})])
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

process :: String -> IO [TopDecl]
process input = case runAlex input parse of
        Left msg -> putStrLn msg >> error msg
        Right res -> print res >> return res
