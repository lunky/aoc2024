module Day5Spec (spec) where

import Day5
import Test.Hspec

spec :: Spec
spec = do
  describe "Day5" $ do
    it "should do sample 1" $ do
      let expected = 143
      day5 _input `shouldBe` expected
  describe "Day5b" $ do
    it "should do sample 1" $ do
      let expected = 123
      day5b _input `shouldBe` expected
