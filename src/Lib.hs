{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
-- import Debug.Trace

readSigned :: String -> Int
readSigned ('+' : s) = read s
readSigned ('-' : s) = - (read s)

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

      go acc (ndx, lst) = acc + (lst !! (ndx `rem` length lst))
      p1 = foldl go 0 . zip [3, 6 ..] . tail
   in p1 . (map . map) xo . lines <$> readFile "src/03-1.txt"

d03_2 :: IO Int
d03_2 =
  let xo '#' = 1
      xo _ = 0
      every2 (_ : y : xs) = y : every2 xs
      every2 _ = []

      go acc (ndx, lst) = acc + (lst !! (ndx `rem` length lst))
      p2 = product . sequence [
            foldl go 0 . zip [1, 2 ..],
            foldl go 0 . zip [3, 6 ..],
            foldl go 0 . zip [5, 10 ..],
            foldl go 0 . zip [7, 14 ..],
            foldl go 0 . zip [1, 2 ..] . every2] . tail
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
d04_2 =
  let nl "" = "\n"
      nl x = x
      valid = all (fieldP . splitOn ":")
        where
          fieldP ("byr" : v : _) = read v `elem` [1920 .. 2002]
          fieldP ("iyr" : v : _) = read v `elem` [2010 .. 2020]
          fieldP ("eyr" : v : _) = read v `elem` [2020 .. 2030]
          fieldP ("hgt" : v : _) = case span isDigit v of
            (d, "cm") -> read d `elem` [150 .. 193]
            (d, "in") -> read d `elem` [59 .. 76]
            _ -> False
          fieldP ("hcl" : v : _) = (length v == 7) && isPrefixOf "#" v
          fieldP ("ecl" : v : _) = v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          fieldP ("pid" : v : _) = (length v == 9) && all isDigit v
   in length
        . filter valid
        . filter ((== 7) . length)
        . map (filter (not . isPrefixOf "cid") . words)
        . lines
        . unwords
        . map nl
        . lines
        <$> readFile "src/04-1.txt"

------------------------------------------------------------------------ 05 --
d05_1 :: IO Int
d05_1 =
  let -- row x y | trace (show x ++ " / " ++ show y) False = undefined
      step a d
        | d `elem` "FL" = take (length a `div` 2) a
        | otherwise = drop (length a `div` 2) a

      row = head . foldl step [0 .. 127]
      col = head . foldl step [0 .. 7]

      calcID (r, c) = row r * 8 + col c
   in maximum
        . map (calcID . span (`elem` "FB"))
        . lines
        <$> readFile "src/05-1.txt"

d05_2 :: IO Int
d05_2 =
  let -- step x y | trace (show x ++ " / " ++ show y) False = undefined
      step a d
        | d `elem` "FL" = take (length a `div` 2) a
        | otherwise = drop (length a `div` 2) a

      row = head . foldl step [0 .. 127]
      col = head . foldl step [0 .. 7]

      calcID (r, c) = row r * 8 + col c

      pairs = zip <*> tail
   in (+1) . fst
        . head
        . filter (\(e1,e2) -> e1-e2 /= -1)
        . pairs
        . sort
        . map (calcID . span (`elem` "FB"))
        . lines
        <$> readFile "src/05-1.txt"

------------------------------------------------------------------------ 06 --
d06_1 = sum . map (length . foldl1 union . lines) . splitOn "\n\n" <$> readFile "src/06-1.txt"
d06_2 = sum . map (length . foldl1 intersect . lines) . splitOn "\n\n" <$> readFile "src/06-1.txt"

------------------------------------------------------------------------ 07 --
d07_1 :: IO Int
d07_1 =
  let shinyGold m = M.foldrWithKey (\color _ acc -> go color + acc) 0 m
        where
          go color
            | "shiny gold" `elem` colors = 1
            | otherwise = foldr (max . go) 0 colors
            where
              colors = m M.! color
      parseK = head . splitOn " bags"
      parseV = filter (/= "other") . map (unwords . tail . words . head . splitOn " bag")
   in shinyGold . M.fromList
        . map (\(x : xs) -> (parseK x, parseV xs))
        . map (concatMap (splitOn ", "))
        . map (splitOn " contain ")
        . lines
        <$> readFile "src/07-1.txt"

d07_2 :: IO Int
d07_2 =
  let shinyGold m = go "shiny gold" - 1
        where
          go color
            | null colors = 1
            | otherwise = foldr (\(clr, cnt) acc -> cnt * go clr + acc) 1 colors
            where
              colors = m M.! color
      parseC = filter (/= "other") . map (unwords . tail . words . head . splitOn " bag")
      parseN = map ((read @Int) . head . words)
      parseV = zip <$> parseC <*> parseN
      parseK = head . splitOn " bags"
   in shinyGold . M.fromList
        . map (\(x : xs) -> (parseK x, parseV xs))
        . map (concatMap (splitOn ", "))
        . map (splitOn " contain ")
        . lines
        <$> readFile "src/07-1.txt"

------------------------------------------------------------------------ 08 --
d08_1 :: IO Int
d08_1 =
  let step acc pc pcs prg
        | pc `elem` pcs = acc
        | otherwise = case splitOn " " (prg !! pc) of
          ("acc" : a : _) -> step (acc + readSigned a) (pc + 1) (pcs ++ [pc]) prg
          ("jmp" : n : _) -> step acc (pc + readSigned n) (pcs ++ [pc]) prg
          _ -> step acc (pc + 1) (pcs ++ [pc]) prg
   in step 0 0 [] . lines <$> readFile "src/08-1.txt"

d08_2 :: IO Int
d08_2 =
  let switch [] = []
      switch xs = case splitOn " " (head xs) of
        ("jmp" : n : _) -> ("nop " ++ n) : tail xs
        ("nop" : n : _) -> ("jmp " ++ n) : tail xs
        _ -> xs
      replace (h, t) = h ++ switch t
      perm n prg = replace . splitAt n $ prg
      combs prg = head . filter (/= -1) . map (\n -> step 0 0 [] . perm n $ prg) $ [0 .. length prg]
      step acc pc pcs prg
        | pc `elem` pcs = -1 -- loop
        | pc >= length prg = acc -- end of program
        | otherwise = case splitOn " " (prg !! pc) of
          ("acc" : a : _) -> step (acc + readSigned a) (pc + 1) (pcs ++ [pc]) prg
          ("jmp" : n : _) -> step acc (pc + readSigned n) (pcs ++ [pc]) prg
          _ -> step acc (pc + 1) (pcs ++ [pc]) prg
   in combs . lines <$> readFile "src/08-1.txt"


someFunc :: IO ()
someFunc = d08_2 >>= print

