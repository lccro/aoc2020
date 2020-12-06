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

    it "day 03" $ do
      d03_1 `shouldReturn` 198
      d03_2 `shouldReturn` 5140884672

    it "day 04" $ do
      d04_1 `shouldReturn` 216
      d04_2 `shouldReturn` 150

    it "day 05" $ do
      d05_1 `shouldReturn` 892
      d05_2 `shouldReturn` 625

    it "day 06" $ do
      d06_1 `shouldReturn` 6542
      d06_2 `shouldReturn` 3299

