module Plato.Test.OpParserSpec where

import Test.Hspec

spec :: Spec
spec = do
        describe "Fixity resolution" $ do
                it "x + y" $ do
                        head [23 ..] `shouldBe` (23 :: Int)
                it "x + y * z" $ do
                        head [23 ..] `shouldBe` (23 :: Int)
                it "x ++ y ++ z" $ do
                        head [23 ..] `shouldBe` (23 :: Int)
                it "x > y > z" $ do
                        head [23 ..] `shouldBe` (23 :: Int)