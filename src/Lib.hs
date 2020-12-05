module Lib where

import Data.List
import Data.List.Split
-- import Debug.Trace

------------------------------------------------------------------------ 01 --
d01_1 :: IO Int
d01_1 =
  let go lst = head [x * y | x <- lst, y <- lst, x + y == 2020]
   in go . map read . lines <$> readFile "src/01-1.txt"

d01_2 :: IO Int
d01_2 =
  let go lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z == 2020]
   in go . map read . lines <$> readFile "src/01-1.txt"

------------------------------------------------------------------------ 02 --
d02_1 :: IO Int
d02_1 =
  let correct (range : char : expr : _) = lower <= cnt && cnt <= upper
        where
          lower = read . head . splitOn "-" $ range
          upper = read . last . splitOn "-" $ range
          cnt = length . filter (== ch) $ expr
          ch = head . head . endBy ":" $ char
   in length . filter correct . map words . lines <$> readFile "src/02-1.txt"

d02_2 :: IO Int
d02_2 =
  let correct (range : char : expr : _) = (ch1 /= ch2) && ((ch1 == ch) || (ch2 == ch))
        where
          lower = read . head . splitOn "-" $ range
          upper = read . last . splitOn "-" $ range
          ch = head . head . endBy ":" $ char
          ch1 = expr !! (lower - 1)
          ch2 = expr !! (upper - 1)
   in length . filter correct . map words . lines <$> readFile "src/02-1.txt"

------------------------------------------------------------------------ 03 --
d03_1 :: IO Int
d03_1 =
  let xo '#' = 1
      xo _ = 0

      go (ndx, lst) acc = acc + (lst !! (ndx `rem` length lst))
      p1 = foldr go 0 . zip [3, 6 ..] . tail
   in p1 . (map . map) xo . lines <$> readFile "src/03-1.txt"

d03_2 :: IO Int
d03_2 =
  let xo '#' = 1
      xo _ = 0
      every2 (_ : y : xs) = y : every2 xs
      every2 _ = []

      go (ndx, lst) acc = acc + (lst !! (ndx `rem` length lst))
      p2 lst =
        product
          [ foldr go 0 . zip [1, 2 ..] . tail $ lst,
            foldr go 0 . zip [3, 6 ..] . tail $ lst,
            foldr go 0 . zip [5, 10 ..] . tail $ lst,
            foldr go 0 . zip [7, 14 ..] . tail $ lst,
            foldr go 0 . zip [1, 2 ..] . every2 . tail $ lst
          ]
   in p2 . (map . map) xo . lines <$> readFile "src/03-1.txt"

------------------------------------------------------------------------ 04 --
d04_1 :: IO Int
d04_1 =
  let nl "" = "\n"
      nl x = x
   in length
        . filter ((== 7) . length)
        . map (filter (not . isPrefixOf "cid") . words)
        . lines
        . unwords
        . map nl
        . lines
        <$> readFile "src/04-1.txt"

d04_2 :: IO Int
d04_2 = return 42

someFunc :: IO ()
someFunc = d04_2 >>= print
