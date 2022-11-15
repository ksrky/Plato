module Plato.Test.ParsingSpec where

import qualified Plato.Test.Parsing.FixResol as F
import qualified Plato.Test.Parsing.Pretty as P

import Test.Hspec

spec :: Spec
spec = do
        describe "Fixity resolution" $ mapM_ F.test F.testcases
        describe "Parsing pretty" $ mapM_ P.test P.testcases