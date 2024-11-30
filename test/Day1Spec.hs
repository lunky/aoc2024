module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "day1" $ do
    it "should work for pattern 1" $ do
      let input = ""
      let expected = 0
      day1 input `shouldBe` expected
  describe "day1b" $ do
    it "should work for pattern 1" $ do
      let expected = 0
      let input = ""
      day1b input `shouldBe` expected
