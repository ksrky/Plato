module Plato.Test.Utils where

import qualified Data.Text as T
import Test.Hspec

shouldSatisfyReturn :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyReturn` pred = action >>= (`shouldSatisfy` pred)