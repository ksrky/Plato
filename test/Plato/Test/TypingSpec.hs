module Plato.Test.TypingSpec where

import Plato.Test.Typing.DeclTransl as DT
import Plato.Test.Typing.ExprTransl as ET
import Plato.Test.Typing.TopDeclTransl as TT
import Plato.Test.Typing.Transl as T
import Plato.Test.Typing.TypeRecon as TR

import Test.Hspec

spec :: Spec
spec = do
        describe "Expr translation" $ mapM_ ET.test ET.testcases
        describe "Decl translation" $ mapM_ DT.test DT.testcases
        describe "TopDecl translation" $ mapM_ TT.test TT.testcases
        describe "Type reconstruction" $ mapM_ TR.test TR.testcases
        describe "translation: file test" $ mapM_ T.test T.testcases
