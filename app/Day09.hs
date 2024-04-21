module Day09 (day09) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = getDataFileName filename >>= readFile

{- | Part 1.  Each line of the input contains a list of readings.
For each list, generate a list of the diffrence between successive entries, then continue until you get to a row of all 0s.  Then use the difference list to predict the next entry in the initial list.  Find the next entry for all of the lists and add them up.
>>> d09p1 "day09-ex.txt"
"TODO"
-}
d09p1 filename = do
  input <- datafile filename
  let histories = map (map read . words) . lines $ input
  return . show . sum . map nextEntry $ histories

nextEntry ns = let difflists =
                     takeWhile
                     (not . all (== 0))
                     (iterate difflist ns)
                   difflist ns = zipWith (-) (tail ns) ns 
               in sum . map last $ difflists
{- | Part 2.  Same as part one, but find a new previous entry for each
   history instead of a new next one.
>>> d09p2 "day09-ex.txt"
"TODO"
-}
d09p2 filename = do
  input <- datafile filename
  let histories = map (reverse . map read . words) . lines $ input
  return . show . sum . map nextEntry $ histories

day09 :: Day
day09 =
  Day
    { dayName = "09",
      dayPart1 = d09p1 "day09.txt",
      dayPart2 = d09p2 "day09.txt"
    }
