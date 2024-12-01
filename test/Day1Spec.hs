module Day1Spec (spec) where

import Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "should do sample 1" $ do
      let expected = 11
      day1 _input `shouldBe` expected
  describe "Day1b" $ do
    it "should do sample 1" $ do
      let expected = 31
      day1b _input `shouldBe` expected
