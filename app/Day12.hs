{-# LANGUAGE NamedFieldPuns #-}

module Day12 (day12) where

import Control.Monad
import Data.List
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = getDataFileName filename >>= readFile

-- | Part 1.  Each line of input has two parts.
--
-- First, a sequence of
-- . # or ? characters,  then a sequence of integers with , between.
--
-- A ? is a misread.  Each ? really represents a . or a #, but we don't
-- know which.
--
-- The numbers are the lengths of groups of # in that line, in order.
-- For example
--
-- #.#.### 1,1,3
-- There are 3 groups of #, with lengths 1, 1, and 3.
--
--
-- ???.### 1,1,3
--
-- Here, we can figure out the the three ? are really #.#, because that's
-- the only way to make the numbers be correct.
--
-- .??..??...?##. 1,1,3
--
-- Here, there are multiple ways to match the pattern.
--
-- .#...#....###. 1,1,3
-- ..#..#....###. 1,1,3
-- .#....#...###. 1,1,3
-- ..#...#...###. 1,1,3
--
-- Our task is to determine the number of possible readings for each line
-- in the input, and add them all up.
--
-- >>> d12p1 "day12-ex.txt"
-- "21"
d12p1 filename = do
  input <- datafile filename
  return . show . sum . map (solutions . parseReading) . lines $ input

data Reading = Reading {chars :: !String, groups :: ![Int]}
  deriving (Show)

parseReading :: String -> Reading
parseReading txt =
  let charPart = takeWhile (/= ' ') txt
      numPart = tail $ dropWhile (/= ' ') txt
   in Reading
        { chars = charPart,
          groups = map read $ commaSplit numPart
        }

unfoldReading :: Reading -> Reading
unfoldReading (Reading chars groups) =
  Reading
    (concat (replicate 5 chars))
    (concat (replicate 5 groups))

delimit :: Char -> String -> [String]
delimit _ [] = []
delimit delimiter txt =
  takeWhile (/= delimiter) txt
    : let rest = dropWhile (/= delimiter) txt
       in if null rest then [] else delimit delimiter (tail rest)

commaSplit = delimit ','

-- Faster version using dynamic programming.
solutions :: Reading -> Int -- ?
solutions (Reading {chars, groups}) =
  solHelper chars groups


solHelper :: String -> [Int] -> Int
solHelper chars groups
  | '?' `notElem` chars = if groupsForChars chars == groups then 1 else 0
  | greedyGroups chars `isPrefixOf` groups = solHelper (subOne '#' chars) groups + solHelper (subOne '.' chars) groups
  | otherwise = 0

subOne :: Char -> String -> String
subOne c s = takeWhile (/= '?') s  ++ [c] ++ drop 1 (dropWhile (/= '?') s)

greedyGroups :: String -> [Int]
greedyGroups s =  case groupsForChars . takeWhile (/= '?') $ s of
                    [] -> []
                    gs -> init gs

groupsForChars = map length . filter ((== '#') . head) . group

groupMatch :: [Int] -> String -> Bool
groupMatch groups chars =
  groups == groupsForChars chars

-- | Part 2.  Short description of the problem.
-- >>> d12p2 "day12-ex.txt"
-- Same as part 1, but everything is five times as long.
d12p2 filename = do
  input <- datafile filename
  return . show . sum . map (solutions . unfoldReading . parseReading) . lines $ input

day12 :: Day
day12 =
  Day
    { dayName = "12",
      dayPart1 = d12p1 "day12.txt",
      dayPart2 = d12p2 "day12-ex.txt"
    }
