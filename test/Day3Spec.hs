module Day3Spec (spec) where

import Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "Day3" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day3 _input `shouldBe` expected
  describe "Day3b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day3b _input `shouldBe` expected
