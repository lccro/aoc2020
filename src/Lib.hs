module Lib where

import Data.List.Split
import Debug.Trace

------------------------------------------------------------------------ 01 --
d01_1 :: IO Int
d01_1 = go . map read . lines <$> readFile "src/01-1.txt"
  where
    go lst = head [x * y | x <- lst, y <- lst, x + y == 2020]

d01_2 :: IO Int
d01_2 = go . map read . lines <$> readFile "src/01-1.txt"
  where
    go lst = head [x * y * z | x <- lst, y <- lst, z <- lst, x + y + z == 2020]

------------------------------------------------------------------------ 02 --
d02_1 :: IO Int
d02_1 = length . filter correct . map words . lines <$> readFile "src/02-1.txt"
  where
    correct lst = lower <= cnt && cnt <= upper
      where
        lower = read . head . splitOn "-" $ (lst !! 0)
        upper = read . last . splitOn "-" $ (lst !! 0)
        cnt = length . filter (== ch) $ expr
        ch = head . head . endBy ":" $ (lst !! 1)
        expr = lst !! 2

d02_2 :: IO Int
d02_2 = length . filter correct . map words . lines <$> readFile "src/02-1.txt"
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
d03_1 :: IO Int
d03_1 =
  let xo '#' = 1
      xo _ = 0
      go (ndx, lst) acc = acc + (lst !! (ndx `rem` (length lst)))
      p1 = foldr go 0 . zip [3, 6 ..] . tail
   in p1 . (map . map) xo . lines <$> readFile "src/03-1.txt"

d03_2 :: IO Int
d03_2 =
  let xo '#' = 1
      xo _ = 0
      go (ndx, lst) acc = acc + (lst !! (ndx `rem` (length lst)))
      every2 (_ : y : xs) = y : every2 xs
      every2 _ = []
      p2 lst =
        product
          [ (foldr go 0 . zip [1, 2 ..] . tail $ lst),
            (foldr go 0 . zip [3, 6 ..] . tail $ lst),
            (foldr go 0 . zip [5, 10 ..] . tail $ lst),
            (foldr go 0 . zip [7, 14 ..] . tail $ lst),
            (foldr go 0 . zip [1, 2 ..] . every2 . tail $ lst)
          ]
   in p2 . (map . map) xo . lines <$> readFile "src/03-1.txt"

someFunc :: IO ()
someFunc = fmap show d03_2 >>= putStrLn
