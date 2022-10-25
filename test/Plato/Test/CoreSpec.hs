module Plato.Test.CoreSpec where

import qualified Plato.Test.Core.KindInfer as KI
import qualified Plato.Test.Core.TermTransl as TT

import Test.Hspec

spec :: Spec
spec = do
        describe "Kind inference" $ mapM_ KI.test KI.testcases
        describe "Term translation" $ mapM_ TT.test TT.testcases
