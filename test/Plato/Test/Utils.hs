module Plato.Test.Utils where

import Test.Hspec

shouldSatisfyReturn :: (HasCallStack, Show a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyReturn` pred = action >>= (`shouldSatisfy` pred)