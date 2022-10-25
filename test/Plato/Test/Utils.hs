module Plato.Test.Utils where

import Plato.Parsing.Lexer
import Plato.Parsing.Monad

import qualified Data.Text as T
import Test.Hspec

shouldSatisfyReturn :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyReturn` pred = action >>= (`shouldSatisfy` pred)