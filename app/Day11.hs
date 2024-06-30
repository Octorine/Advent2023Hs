{-# LANGUAGE NumericUnderscores #-}

module Day11 (day11) where

import Data.List (transpose)
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = getDataFileName filename >>= readFile

-- | Part 1.  Input is a grid containing . and # characters.  Find the
-- sum of the lengths of the shortest paths between every pair of #.
-- But first, any rows or columns consisting completely of . should be
-- doubled
--
-- >>> d11p1 "day11-ex.txt"
-- "374"
d11p1 filename = do
  input <- datafile filename
  let result = findDistances . transpose . doubleRows . transpose . doubleRows . lines $ input :: Int
  return $ show result

doubleRows rows = rows >>= doubleIfEmpty
  where
    doubleIfEmpty r = if all (== '.') r then [r, r] else [r]

allCoords :: (Enum n, Num n) => [String] -> [[(n, n, Char)]]
allCoords = zipWith swizzle nats . map (zip nats)
  where
    nats :: (Num n, Enum n) => [n]
    nats = [1 ..]
    swizzle :: (Num n) => n -> [(n, Char)] -> [(n, n, Char)]
    swizzle y = map (inject y)
    inject :: (Num n) => n -> (n, Char) -> (n, n, Char)
    inject y (x, v) = (x, y, v)

findDistances :: (Num n, Enum n, Integral n) => [String] -> n
findDistances rows =
  let coords = filter (\(x, y, v) -> v == '#') . concat . allCoords $ rows
   in sum
        ( do
            (x1, y1, v1) <- coords
            (x2, y2, v2) <- coords
            return $ if v1 == '#' && v2 == '#' then abs (x1 - x2) + abs (y1 - y2) else 0
        )
        `div` 2

-- | Part 2. Same as before, but some of the letters are digits spelled
--   out. Now for each line take the first digit or spelled-out digit
--   and the last spelled-out digit, then make two-digit numbers and add
--   them up like in step one.
--
-- >>> d01p2 "day01p2-ex.txt"
-- "281"
d11p2 filename = do
  input <- datafile filename
  let result = findDistances2 . lines $ input :: Integer
  return $ show result

findDistances2 :: [String] -> Integer
findDistances2 rows =
  let coords = filter (\(x, y, v) -> v == '#') . concat . allCoords $ rows :: [(Integer, Integer, Char)]
      multFactor = 1_000_000 :: Integer
      emptyRows = findEmpty rows
      emptyCols = findEmpty $ transpose rows
      coords' = map (updateCoords emptyRows emptyCols) coords
      updateCoords er ec (x, y, v) =
        ( x + fromIntegral (length (filter (< x) ec)) * (multFactor - 1),
          y + fromIntegral (length (filter (< y) er)) * (multFactor - 1),
          v
        )
   in sum
        ( do
            (x1, y1, v1) <- coords'
            (x2, y2, v2) <- coords'
            return $ if v1 == '#' && v2 == '#' then abs (x1 - x2) + abs (y1 - y2) else 0
        )
        `div` 2

findEmpty :: [String] -> [Integer]
findEmpty = map fst . filter isEmpty . zip [1 ..]
  where
    isEmpty (rowNum, row) = all (== '.') row

day11 :: Day
day11 =
  Day
    { dayName = "11",
      dayPart1 = d11p1 "day11.txt",
      dayPart2 = d11p2 "day11.txt"
    }
