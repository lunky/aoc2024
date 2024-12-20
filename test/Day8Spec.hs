module Day8Spec (spec) where

import Day8
import Test.Hspec

spec :: Spec
spec = do
  describe "Day8" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day8 _input `shouldBe` expected
  describe "Day8b" $ do
    xit "should do sample 1" $ do
      let expected = 0
      day8b _input `shouldBe` expected
