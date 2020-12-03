module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
  describe "all" $ do
    it "day 01" $ do
      d01_1 `shouldReturn` 138379
      d01_2 `shouldReturn` 85491920

    it "day 02" $ do
      d02_1 `shouldReturn` 445
      d02_2 `shouldReturn` 491
