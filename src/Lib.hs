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
d03_1 = go 3 0 . drop 1 . lines <$> readFile "src/03-1.txt"
  where
    go i acc (x : xs)
      -- | trace ("go " ++ show i ++ " " ++ show x) False = undefined
      | null xs = acc + xo
      | otherwise = go (rem (i + 3) (length x)) (acc + xo) xs
      where xo = if (x !! i) == '#' then 1 else 0

d03_2 :: IO Int
d03_2 = p2 . lines <$> readFile "src/03-1.txt"
  where
    p2 lst = product [(go1 1 1 0 t),(go1 3 3 0 t),(go1 5 5 0 t),(go1 7 7 0 t),(go2 1 1 0 tt)]
      where t = drop 1 lst
            tt = drop 2 lst
    go1 inc i acc (x : xs)
      -- | trace ("go1 " ++ show i ++ " " ++ show acc ++ " " ++ show xo) False = undefined
      | null xs = acc + xo
      | otherwise = go1 inc (rem (i + inc) (length x)) (acc + xo) xs
      where xo = if (x !! i) == '#' then 1 else 0
    go2 inc i acc (x : xs)
      -- | trace ("go2 " ++ show i ++ " " ++ show acc ++ " " ++ show xo) False = undefined
      | null xs = acc + xo
      | otherwise = go2 inc (rem (i + inc) (length x)) (acc + xo) (tail xs)
      where xo = if (x !! i) == '#' then 1 else 0

someFunc :: IO ()
someFunc = fmap show d03_2 >>= putStrLn
