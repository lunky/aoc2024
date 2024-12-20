module Day2Spec (spec) where

import Day2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ do
    it "should do sample 1" $ do
      let expected = 2
      day2 _input `shouldBe` expected
  describe "Day2b" $ do
    it "should do sample 1" $ do
      let expected = 4
      day2b _input `shouldBe` expected
