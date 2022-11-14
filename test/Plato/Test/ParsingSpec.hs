module Plato.Test.ParsingSpec where

import qualified Plato.Test.Parsing.FixResol as F

import Test.Hspec

spec :: Spec
spec = do
        describe "Fixity resolution" $ mapM_ F.test F.testcases