module Plato.Test.TypingSpec where

-- import Plato.Test.Typing.DeclTransl as DT
import Plato.Test.Typing.ExprTransl as ET

{-import Plato.Test.Typing.Pretty as P
import Plato.Test.Typing.Rename as R
import Plato.Test.Typing.TopDeclPretty as TP
import Plato.Test.Typing.TopDeclTransl as TT
import Plato.Test.Typing.Transl as T
import Plato.Test.Typing.TypeCheck as TC-}

import Test.Hspec

spec :: Spec
spec = do
        describe "Expr translation" $ mapM_ ET.test ET.testcases

{-describe "Decl translation" $ mapM_ DT.test DT.testcases
describe "TopDecl translation" $ mapM_ TT.test TT.testcases
describe "Type reconstruction" $ mapM_ TC.test TC.testcases
describe "Translation: file test" $ mapM_ T.test T.testcases
describe "Renaming test" $ mapM_ R.test R.testcases
describe "TopDecl pretty test" $ mapM_ TP.test TP.testcases
describe "Pretty test" $ mapM_ P.test P.testcases-}