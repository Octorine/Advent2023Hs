module Day01 (day01) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = getDataFileName filename >>= readFile

{- | Part 1.  Short description of the problem.
>>> d01p1 "day01-ex.txt"
"TODO"
-}
d01p1 filename = do
  input <- datafile filename
  return "Day 01 Part 1 result"

{- | Part 2.  Short description of the problem.
>>> d01p2 "day01-ex.txt"
"TODO"
-}
d01p2 filename = do
  input <- datafile filename
  return "Day 01 Part 1 result"


day01 :: Day
day01 =
  Day
    { dayName = "01",
      dayPart1 = d01p1 "day01.txt",
      dayPart2 = d01p1 "day01.txt"
    }
