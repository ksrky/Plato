module Plato.KindInferSpec where

import Plato.Common.Info
import Plato.Common.Name
import Plato.IR.Syntax
import Plato.Translation.KindInfer

import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Eval" $ mapM_ test testcases

di :: Info
di = dummyInfo

abst :: String -> Type -> Type
abst x = AbsType di (str2tyVarName x)

appl :: Type -> Type -> Type
appl = AppType di

x :: Type
x = VarType di (str2tyVarName "x")

f :: Type
f = VarType di (str2tyVarName "f")

g :: Type
g = VarType di (str2tyVarName "g")

sumt :: [(String, [Type])] -> Type
sumt xs = SumType (map (\(x, tys) -> (di, str2tyVarName x, tys)) xs)

(-->) :: Kind -> Kind -> Kind
(-->) = KArr

infixr 9 -->

testcases :: [(String, Type, Kind -> Expectation)]
testcases =
        [ ("\\x -> x", abst "x" x, (`shouldBe` KStar --> KStar))
        , ("\\f -> \\x -> f x", abst "f" (abst "x" (appl f x)), (`shouldBe` (KStar --> KStar) --> KStar --> KStar))
        ,
                ( "\\f -> \\g -> \\x -> T (g f) (f x)"
                , abst "f" (abst "g" (abst "x" (sumt [("T", [appl g f, appl f x])])))
                , (`shouldBe` (KStar --> KStar) --> ((KStar --> KStar) --> KStar) --> KStar --> KStar)
                )
        ]

test :: (String, Type, Kind -> Expectation) -> SpecWith ()
test (str, ty, iscorrect) = it str $ iscorrect (kindInfer ty)