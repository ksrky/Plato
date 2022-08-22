module Plato.KindInferSpec where

import Plato.Common.Info
import Plato.Common.Name
import Plato.Common.Vect
import Plato.Core.Context
import Plato.Core.Syntax
import Plato.IR.Syntax
import Plato.Translation.KindInfer

import Test.Hspec

spec :: Spec
spec = do
        describe "Plato.Eval" $ mapM_ test testcases

di :: Info
di = dummyInfo

vart :: String -> Type
vart x = VarType di (str2tyVarName x)

cont :: String -> Type
cont x = VarType di (str2tyConName x)

abst :: String -> Type -> Type
abst x = AbsType di (str2tyVarName x)

appl :: Type -> Type -> Type
appl = AppType di

x :: Type
x = vart "x"

a :: Type
a = vart "a"

f :: Type
f = VarType di (str2tyVarName "f")

g :: Type
g = VarType di (str2tyVarName "g")

sumt :: [(String, [Type])] -> Type
sumt xs = SumType (map (\(x, tys) -> (di, str2tyVarName x, tys)) xs)

listt :: Type
listt = RecType di (str2tyVarName "x") (abst "a" (appl (vart "List") a))

(-->) :: Kind -> Kind -> Kind
(-->) = KnArr

star :: Kind
star = KnStar

infixr 9 -->

testcases :: [(String, Type, Kind -> Expectation)]
testcases =
        [ ("\\x -> x", abst "x" x, (`shouldBe` star --> star))
        , ("\\f -> \\x -> f x", abst "f" (abst "x" (appl f x)), (`shouldBe` (star --> star) --> star --> star))
        ,
                ( "\\f -> \\g -> \\x -> T (g f) (f x)"
                , abst "f" (abst "g" (abst "x" (sumt [("T", [appl g f, appl f x])])))
                , (`shouldBe` (star --> star) --> ((star --> star) --> star) --> star --> star)
                )
        , ("\\x -> C x", abst "x" (appl (cont "C") x), (`shouldBe` star --> star))
        ]

initContext :: Context
initContext = cons (str2tyConName "C", TyVarBind (star --> star)) emptyContext

test :: (String, Type, Kind -> Expectation) -> SpecWith ()
test (str, ty, iscorrect) = it str $ iscorrect (kindInfer initContext ty)
