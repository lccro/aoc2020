module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
  describe "all" $ do
    it "day 01" $ do
      _01_1 `shouldReturn` 138379
      _01_2 `shouldReturn` 85491920

    it "day 02" $ do
      _02_1 `shouldReturn` 445
      _02_2 `shouldReturn` 491
