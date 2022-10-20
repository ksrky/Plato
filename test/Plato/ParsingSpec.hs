module Plato.ParsingSpec where

import Plato.Common.Name
import Plato.Common.SrcLoc
import Plato.Parsing.Fixity
import Plato.Parsing.Parser
import Plato.Parsing.Resolver
import Plato.Syntax.Parsing

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Plato.Common.Error
import Plato.Parsing.Monad
import Test.Hspec

spec :: Spec
spec =
        describe "Plato.Eval" $ mapM_ test testcases

x :: Located Expr
x = noLoc $ VarE (noLoc $ str2varName "x")

y :: Located Expr
y = noLoc $ VarE (noLoc $ str2varName "y")

z :: Located Expr
z = noLoc $ VarE (noLoc $ str2varName "z")

plus :: Op
plus = Op (noLoc $ str2varName "+") 6 Leftfix

times :: Op
times = Op (noLoc $ str2varName "*") 7 Leftfix

con :: Op
con = Op (noLoc $ str2conName "++") 5 Leftfix

gt :: Op
gt = Op (noLoc $ str2varName ">") 4 Nonfix

testcases :: [(String, IO Expr -> Expectation)]
testcases =
        [ ("x + y", (`shouldReturn` OpE x (noLoc $ str2varName "+") y))
        , ("x + y * z", (`shouldReturn` OpE x (noLoc $ str2varName "+") (noLoc $ OpE y (noLoc $ str2varName "*") z)))
        , ("x ++ y ++ z", (`shouldReturn` OpE (noLoc $ OpE x (noLoc $ str2varName "++") y) (noLoc $ str2varName "++") z))
        , ("x > y > z", (`shouldThrow` anyException))
        ]

test :: (String, IO Expr -> Expectation) -> SpecWith ()
test (inp, iscorrect) = it inp $
        iscorrect $ do
                (res, st) <- eitherToMonadThrow (parse (T.pack inp) exprParser)
                let opdict = opDict (parser_ust st)
                unLoc <$> resolve opdict res
