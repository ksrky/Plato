module Plato.Test.CoreSpec where

import qualified Plato.Test.Core.Eval as E
import qualified Plato.Test.Core.KindInfer as KI
import qualified Plato.Test.Core.Pretty as P

import Test.Hspec

spec :: Spec
spec = do
        describe "Kind inference" $ mapM_ KI.test KI.testcases
        describe "Pretty test" $ mapM_ P.test P.testcases
        describe "Evaluation test" $ mapM_ E.test E.testcases

{-describe "Term translation" $ mapM_ TT.test TT.testcases
describe "Pretty term" $ mapM_ PT.test PT.testcases -}