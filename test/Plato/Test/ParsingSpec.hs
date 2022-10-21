module Plato.Test.ParsingSpec where

import qualified Plato.Test.FixResol as F

import Test.Hspec

spec :: Spec
spec =
        describe "Fixity resolution" $ mapM_ F.test F.testcases
