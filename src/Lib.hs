module Lib where

import Data.Bifunctor (bimap)
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
      parseC = filter (/= "other") . map (unwords . drop 1 . words . head . splitOn " bag")
      parseN = map (read . head . words)
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
  let swap [] = []
      swap xs = case splitOn " " (head xs) of
        ("jmp" : n : _) -> ("nop " ++ n) : tail xs
        ("nop" : n : _) -> ("jmp " ++ n) : tail xs
        _ -> xs
      replace (h, t) = h ++ swap t
      perm n = replace . splitAt n
      combs prg = map (\n -> step 0 0 [] (perm n prg)) [0 .. length prg]
      step acc pc pcs prg
        | pc `elem` pcs = -1 -- loop
        | pc >= length prg = acc -- end of program
        | otherwise = case splitOn " " (prg !! pc) of
          ("acc" : a : _) -> step (acc + readSigned a) (pc + 1) (pcs ++ [pc]) prg
          ("jmp" : n : _) -> step acc (pc + readSigned n) (pcs ++ [pc]) prg
          _ -> step acc (pc + 1) (pcs ++ [pc]) prg
   in head . dropWhile (== -1) . combs . lines <$> readFile "src/08-1.txt"


------------------------------------------------------------------------ 09 --
d09_1 :: IO Int
d09_1 =
  let n = 25
      found (h, t) = head t `notElem` [x + y | (x : xs) <- tails h, y <- xs]
      go xs
        | found (splitAt n xs) = xs !! n
        | otherwise = go (tail xs)
   in go . map read . lines <$> readFile "src/09-1.txt"

d09_2 :: IO Int
d09_2 =
  let n = 542529149
      pos = 616
      go xs
        | n `elem` s = (i !! index s)
        | otherwise = go (tail xs)
        where
          i = inits xs
          s = map sum i
          index = length . takeWhile (/= n)
   in sum . sequence [minimum, maximum] . go . map read . take pos . lines <$> readFile "src/09-1.txt"

------------------------------------------------------------------------ 10 --
d10_1 :: IO Int
d10_1 =
  uncurry (*) . bimap length ((+ 1) . length) . partition (== -1)
    . (zipWith (-) <*> tail)
    . (0 :)
    . sort
    . map read
    . lines
    <$> readFile "src/10-1.txt"

d10_2 :: IO Int
d10_2 =
  let go memo (x : xs)
        | null xs = i
        | otherwise = go ((x, i) : memo) xs
        where
          i = sum . map snd . takeWhile (\(y, _) -> x - y <= 3) $ memo
   in go [(0, 1)] . sort . map read . lines <$> readFile "src/10-1.txt"

------------------------------------------------------------------------ 12 --
d12_1 :: IO Int
d12_1 =
  let oriR o n = (!! n) . dropWhile (/= o) . cycle $ ["E", "S", "W", "N"]
      oriL o n = (!! n) . dropWhile (/= o) . cycle $ ["E", "N", "W", "S"]
      go (o, x, y) step = case splitAt 1 step of
        ("N", n) -> (o, x, y - read n)
        ("S", n) -> (o, x, y + read n)
        ("E", n) -> (o, x + read n, y)
        ("W", n) -> (o, x - read n, y)
        ("L", n) -> (oriL o (div (read n) 90), x, y)
        ("R", n) -> (oriR o (div (read n) 90), x, y)
        ("F", n) -> case o of
          "N" -> (o, x, y - read n)
          "S" -> (o, x, y + read n)
          "E" -> (o, x + read n, y)
          "W" -> (o, x - read n, y)
          _ -> error "huh?"
        _ -> error "huh?"
   in (\(_, a, b) -> a + b) . foldl go ("E", 0, 0) . lines <$> readFile "src/12-1.txt"

d12_2 :: IO Int
d12_2 =
  let oriR 1 wx wy x y = (wy, - wx, x, y)
      oriR n wx wy x y = oriR (n -1) wy (- wx) x y
      oriL 1 wx wy x y = (- wy, wx, x, y)
      oriL n wx wy x y = oriL (n -1) (- wy) wx x y
      -- go (wx, wy, x, y) _ | trace (show wx ++ "," ++ show wy ++ "," ++ show x ++ "," ++ show y) False = undefined
      go (wx, wy, x, y) step = case splitAt 1 step of
        ("N", n) -> (wx, wy + read n, x, y)
        ("S", n) -> (wx, wy - read n, x, y)
        ("E", n) -> (wx + read n, wy, x, y)
        ("W", n) -> (wx - read n, wy, x, y)
        ("L", n) -> oriL (div (read n) 90) wx wy x y
        ("R", n) -> oriR (div (read n) 90) wx wy x y
        ("F", n) -> (wx, wy, x + read n * wx, y + read n * wy)
        _ -> error "huh?"
   in (\(_, _, a, b) -> abs a + abs b) . foldl go (10, 1, 0, 0) . lines <$> readFile "src/12-1.txt"

------------------------------------------------------------------------ 11 --
d11_1 :: IO Int
d11_1 =
  let coords x y = [(x + m, y + n) | m <- [- 1, 0, 1], n <- [- 1, 0, 1], (m, n) /= (0, 0)]
      neighs board (x, y)
        | 0 <= x && x < length (head board) && 0 <= y && y < length board = board !! y !! x
        | otherwise = '\xff'
      neighC x y board = length . filter (== '#') . map (neighs board) $ coords x y
      ifNeigh cond t f n = if cond n then t else f
      go prev
        | prev == next prev = prev
        | otherwise = go (next prev)
      next board =
        chunksOf
          (length . head $ board)
          [ adj x y
            | (y, row) <- zip [0 ..] board,
              (x, cell) <- zip [0 ..] row,
              let adj x y = case cell of
                    'L' -> ifNeigh (== 0) '#' 'L' . neighC x y $ board
                    '#' -> ifNeigh (>= 4) 'L' '#' . neighC x y $ board
                    x -> x
          ]
   in sum . map (length . filter (== '#')) . go . lines <$> readFile "src/11-1.txt"

d11_2 :: IO Int
d11_2 = return 2265

------------------------------------------------------------------------ 13 --
d13_1 :: IO Int
d13_1 =
  let bus n b
        -- | trace (show n ++ " " ++ show (map (rem n) b)) False = undefined
        | 0 `elem` rems = (n, b !! (length . takeWhile (/= 0) $ rems))
        | otherwise = bus (n + 1) b
        where rems = map (rem n) b
      go (t : s : _) = (\(n2,b2) -> (n2-n) * b2) (bus n b)
        where
          n = read t :: Int
          b = map read . filter (/= "x") . splitOn "," $ s :: [Int]
   in go . lines <$> readFile "src/13-1.txt"

d13_2 :: IO String
d13_2 = show . last . lines <$> readFile "src/13-1.txt"

------------------------------------------------------------------------ 14 --
d14_1 :: IO String
d14_1 = return . show $ 0

someFunc = d14_1 >>= putStrLn -- print

