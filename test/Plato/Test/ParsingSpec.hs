module Plato.Test.ParsingSpec where

import qualified Plato.Test.Parsing.FixResol as F
import qualified Plato.Test.Parsing.Parser as P

import Test.Hspec

spec :: Spec
spec = do
        describe "Fixity resolution" $ mapM_ F.test F.testcases
        describe "Parser test" $ mapM_ P.test P.testcases
