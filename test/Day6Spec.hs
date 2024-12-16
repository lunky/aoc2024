module Day6Spec (spec) where

import Day6
import Test.Hspec

spec :: Spec
spec = do
  describe "Day6" $ do
    it "should do sample 1" $ do
      let expected = 41
      day6 _input `shouldBe` expected
  describe "Day6b" $ do
    it "should do sample 1" $ do
      let expected = 6
      day6b _input `shouldBe` expected
