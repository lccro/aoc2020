{-# LANGUAGE TypeApplications #-}

module Lib
    ( someFunc
    ) where

import Data.List.Split

_01 :: IO String
_01 = (show @Int) . go . map read . lines <$> readFile "src/01-1.txt"
  where
    go :: [Int] -> Int
    -- go ys = head [x * y | x <- ys, y <- ys, x + y == 2020]
    go ys = head [x * y * z | x <- ys, y <- ys, z <- ys, x + y + z == 2020]

_02 :: IO String
_02 = (show @Int) . length . filter right . map words . lines <$> readFile "src/02-1.txt"
  where
    right :: [String] -> Bool
    -- right ys = lower <= cnt && cnt <= upper
    right ys =
      (e !! (lower - 1) == c) && (e !! (upper - 1) /= c)
        || (e !! (lower - 1) /= c) && (e !! (upper - 1) == c)
      where
        -- cnt = length . filter (==c) $ e
        lower = read . head . splitOn "-" $ (ys !! 0)
        upper = read . last . splitOn "-" $ (ys !! 0)
        c = head . head . endBy ":" $ (ys !! 1)
        e = ys !! 2

someFunc :: IO ()
someFunc = _02 >>= putStrLn
