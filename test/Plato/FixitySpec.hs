module Plato.FixitySpec where

import Plato.Abstract.Fixity
import Plato.Abstract.Syntax
import Plato.Common.Info
import Plato.Common.Name

import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Eval" $ mapM_ test testcases

x :: Exp
x = Exp (VarExpr dummyInfo (str2varName "x"))

y :: Exp
y = Exp (VarExpr dummyInfo (str2varName "y"))

z :: Exp
z = Exp (VarExpr dummyInfo (str2varName "z"))

plus :: Op
plus = Op (str2varName "+") 6 Leftfix

times :: Op
times = Op (str2varName "*") 7 Leftfix

con :: Op
con = Op (str2conName "++") 5 Leftfix

gt :: Op
gt = Op (str2varName ">") 4 Nonfix

testcases :: [(String, [Tok], Maybe Exp -> Expectation)]
testcases =
        [ ("x + y", [TExp x, TOp dummyInfo plus [], TExp y], (`shouldBe` Just (OpApp dummyInfo x plus [] y)))
        , ("x + y * z", [TExp x, TOp dummyInfo plus [], TExp y, TOp dummyInfo times [], TExp z], (`shouldBe` Just (OpApp dummyInfo x plus [] (OpApp dummyInfo y times [] z))))
        , ("x ++ y ++ z", [TExp x, TOp dummyInfo con [], TExp y, TOp dummyInfo con [], TExp z], (`shouldBe` Just (OpApp dummyInfo (OpApp dummyInfo x con [] y) con [] z)))
        , ("x > y > z", [TExp x, TOp dummyInfo gt [], TExp y, TOp dummyInfo gt [], TExp z], (`shouldBe` Nothing))
        ]

test :: (String, [Tok], Maybe Exp -> Expectation) -> SpecWith ()
test (str, toks, iscorrect) = it str $ iscorrect (resolve toks)