module Lib where

import Data.List.Split

------------------------------------------------------------------------ 01 --
_01_1 :: IO Int
_01_1 = go . map read . lines <$> readFile "src/01-1.txt"
  where
    go lst = head [x * y | x <- lst, y <- lst, x + y == 2020]

_01_2 :: IO Int
_01_2 = go . map read . lines <$> readFile "src/01-1.txt"
  where
    go lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z == 2020]

------------------------------------------------------------------------ 02 --
_02_1 :: IO Int
_02_1 = length . filter correct . map words . lines <$> readFile "src/02-1.txt"
  where
    correct lst = lower <= cnt && cnt <= upper
      where
        lower = read . head . splitOn "-" $ (lst !! 0)
        upper = read . last . splitOn "-" $ (lst !! 0)
        cnt = length . filter (== ch) $ expr
        ch = head . head . endBy ":" $ (lst !! 1)
        expr = lst !! 2

_02_2 :: IO Int
_02_2 = length . filter correct . map words . lines <$> readFile "src/02-1.txt"
  where
    correct lst = (ch1 /= ch2) && ((ch1 == ch) || (ch2 == ch))
      where
        lower = read . head . splitOn "-" $ (lst !! 0)
        upper = read . last . splitOn "-" $ (lst !! 0)
        ch = head . head . endBy ":" $ (lst !! 1)
        ch1 = expr !! (lower - 1)
        ch2 = expr !! (upper - 1)
        expr = lst !! 2

------------------------------------------------------------------------ 03 --
_03_1 :: IO Int
_03_1 = return 3

someFunc :: IO ()
someFunc = fmap show _03_1 >>= putStrLn
