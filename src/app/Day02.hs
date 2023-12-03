module Day02 (day02) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = getDataFileName filename >>= readFile

{- | Part 1.  Short description of the problem.
>>> d02p1 "day02-ex.txt"
"TODO"
-}
d02p1 filename = do
  input <- datafile filename
  return "Day 02 Part 1 result"

{- | Part 2.  Short description of the problem.
>>> d02p2 "day02-ex.txt"
"TODO"
-}
d02p2 filename = do
  input <- datafile filename
  return "Day 02 Part 1 result"


day02 :: Day
day02 =
  Day
    { dayName = "02",
      dayPart1 = d02p1 "day02.txt",
      dayPart2 = d02p1 "day02.txt"
    }
