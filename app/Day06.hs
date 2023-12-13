module Day06 (day06) where

import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  Short description of the problem.
-- >>> d06p1 "day06-ex.txt"
-- "288"
d06p1 filename = do
  input <- datafile filename
  return . show . product . map wins $ parseInput input

-- | Part 2.  Short description of the problem.
-- >>> d06p2 "day06-ex.txt"
-- "71503"
d06p2 filename = do
  input <- datafile filename
  return . show . product . map wins $ parseInput2 input

day06 :: Day
day06 =
  Day
    { dayName = "06",
      dayPart1 = d06p1 "day06.txt",
      dayPart2 = d06p2 "day06.txt"
    }

data Race = Race {time :: Int, distance :: Int}
  deriving (Show)

parseInput input =
  let [timeLine, distLine] = lines input
   in zipWith Race (drop 1 . map read $ words timeLine) (drop 1 . map read $ words distLine)

tryPress :: Race -> Int -> Int
tryPress race pressTime = max 0 ((time race - pressTime) * pressTime)

tryAllTimes :: Race -> [Int]
tryAllTimes r = map (tryPress r) [0 .. time r]

wins :: Race -> Int
wins r = length . filter (> distance r) $ tryAllTimes r

parseInput2 input =
  let [timeLine, distLine] = lines input
   in [Race (read . concat . drop 1 $ words timeLine) (read . concat . drop 1 $ words distLine)]
