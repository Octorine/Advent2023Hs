module Day03 (day03) where

import Data.Char (isDigit)
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  Input is a 2d array of characters containing some numbers, some symbols and some . characters.  Add up all the numbers that are adjacent to at least one symbol (not counting .).
-- >>> d03p1 "day03-ex.txt"
-- "4361"
d03p1 filename = do
  input <- datafile filename
  let rows = zip [0 ..] (lines input)
  let (symbols, numbers) = processRows rows
  return
    . show
    . sum
    . map snd
    . filter (adjascencyFilter symbols)
    $ numbers

-- | Part 2.  Short description of the problem.
-- >>> d03p2 "day03-ex.txt"
-- "467835"
d03p2 filename = do
  input <- datafile filename
  let rows = zip [0 ..] (lines input)
  let (symbols, numbers) = processRows rows
  return . show . sum . map (gearRatio numbers) $ symbols

day03 :: Day
day03 =
  Day
    { dayName = "03",
      dayPart1 = d03p1 "day03.txt",
      dayPart2 = d03p2 "day03.txt"
    }

type Coords = (Int, Int)

processRows :: [(Int, String)] -> ([(Coords, Char)], [(Coords, Int)])
processRows rows = go rows [] []
  where
    go [] sym num = (sym, num)
    go (row : rest) sym num =
      let (sym', num') = processRow row sym num
       in go rest sym' num'

processRow :: (Int, String) -> [(Coords, Char)] -> [(Coords, Int)] -> ([(Coords, Char)], [(Coords, Int)])
processRow (rowIndex, row) symbols numbers =
  go (zip [0 ..] row) symbols numbers
  where
    go [] symbols numbers = (symbols, numbers)
    go ((columnIndex, c) : cs) s n =
      if c == '.'
        then go cs s n
        else
          if isDigit c
            then
              let numString = c : (map snd . takeWhile (isDigit . snd) $ cs)
                  num = read numString
                  cs' = drop (length numString - 1) cs
               in go cs' s (((rowIndex, columnIndex), num) : n)
            else go cs (((rowIndex, columnIndex), c) : s) n

isAdjascent :: (Coords, Int) -> (Coords, Char) -> Bool
isAdjascent ((ny, nx), number) ((sy, sx), symbol) =
  abs (ny - sy) <= 1
    && nx <= sx + 1
    && nx + (length (show number)) - 1 >= sx - 1

adjascencyFilter :: [(Coords, Char)] -> (Coords, Int) -> Bool
adjascencyFilter symbols number =
  any (isAdjascent number) symbols

gearRatio :: [(Coords, Int)] -> (Coords, Char) -> Int
gearRatio numbers symbol =
  let ns = filter (\n -> isAdjascent n symbol) numbers
   in if length ns < 2
        then 0
        else product . map snd . filter (\n -> isAdjascent n symbol) $ numbers
