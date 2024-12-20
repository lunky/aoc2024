module Day7Spec (spec) where

import Day7
import Test.Hspec

spec :: Spec
spec = do
  describe "Day7" $ do
    it "should do sample 1" $ do
      let expected = 3749
      day7 _input `shouldBe` expected
  describe "Day7b" $ do
    it "should do sample 1" $ do
      let expected = 11387
      day7b _input `shouldBe` expected
