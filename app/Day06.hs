module Day06 (day06) where

import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  The input describes a series of races.  The first line
-- describes the time limit for each race, an dthe second line lists
-- the best distance.  The challenge is to try running each race at
-- different speeds, with the tradeoff that faster speeds mean reduced
-- time limit.  Find the number of ways to win each race, then
-- multiply all the results together.

-- >>> d06p1 "day06-ex.txt"
-- "288"
d06p1 filename = do
  input <- datafile filename
  return . show . product . map wins $ parseInput input

-- | Part 2.  Like part one, but the input only describes one race.
-- The numbers in the time and distance lists need to be concatenated
-- together

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
