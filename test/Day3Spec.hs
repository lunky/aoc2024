module Day3Spec (spec) where

import Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "Day3" $ do
    it "should do sample 1" $ do
      let expected = 161
      day3 _input `shouldBe` expected
  describe "Day3b" $ do
    it "should do sample 1" $ do
      let expected = 48
      day3b _input2 `shouldBe` expected
