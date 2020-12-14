module LibSpec where

import Lib

import Test.Hspec

spec :: Spec
spec = do
  describe "all" $ do
    it "day 13" $ do
      d13_1 `shouldReturn` 102
      d13_2 `shouldReturn` 102

    it "day 11" $ do
      d11_1 `shouldReturn` 2265
      d11_2 `shouldReturn` 2265

    it "day 12" $ do
      d12_1 `shouldReturn` 998
      d12_2 `shouldReturn` 71586

    it "day 10" $ do
      d10_1 `shouldReturn` 2277
      d10_2 `shouldReturn` 37024595836928

    it "day 09" $ do
      d09_1 `shouldReturn` 542529149
      d09_2 `shouldReturn` 75678618

    it "day 08" $ do
      d08_1 `shouldReturn` 1814
      d08_2 `shouldReturn` 1056

    it "day 07" $ do
      d07_1 `shouldReturn` 337
      d07_2 `shouldReturn` 50100

    it "day 06" $ do
      d06_1 `shouldReturn` 6542
      d06_2 `shouldReturn` 3299

    it "day 05" $ do
      d05_1 `shouldReturn` 892
      d05_2 `shouldReturn` 625

    it "day 04" $ do
      d04_1 `shouldReturn` 216
      d04_2 `shouldReturn` 150

    it "day 03" $ do
      d03_1 `shouldReturn` 198
      d03_2 `shouldReturn` 5140884672

    it "day 02" $ do
      d02_1 `shouldReturn` 445
      d02_2 `shouldReturn` 491

    it "day 01" $ do
      d01_1 `shouldReturn` 138379
      d01_2 `shouldReturn` 85491920

