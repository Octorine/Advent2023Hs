{-# LANGUAGE NamedFieldPuns #-}

module Day04 (day04) where

import Data.Char (isDigit)
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  Input consists of a list of numbers with a | in the middle.  For each line, find all the numbers on the left that also appear on the right and award 2^(number minus one) points for that line.  Add up the lines.
-- >>> d04p1 "day04-ex.txt"
-- "13"
d04p1 filename = do
  input <- datafile filename
  return . show . sum . map (points . readRecord) . lines $ input

-- | Part 2.  Find intersections in input as before, but now the number of intersections in each record represents extra copies of that number of following records.
-- >>> d04p2 "day04-ex.txt"
-- "30"
d04p2 filename = do
  input <- datafile filename
  let records =
        map readRecord
          . lines
          $ input
      scores =
        M.fromList
          . map (\r -> (name r, intersections r))
          $ records
      last = maximum $ name <$> records
      result = go [1 .. last] 0
      go rs total =
        let first = minimum rs
            mult = length . filter (== first) $ rs
            score = fromJust (M.lookup first scores)
            newRs =
              ( filter (/= first) rs
                  ++ concat
                    ( replicate
                        mult
                        [ first + 1
                          .. ( min
                                 last
                                 (first + score)
                             )
                        ]
                    )
              )
         in total `seq`
              if first >= last || null newRs
                then total + mult
                else
                  go
                    newRs
                    (total + mult)

  return . show $ result

day04 :: Day
day04 =
  Day
    { dayName = "04",
      dayPart1 = d04p1 "day04.txt",
      dayPart2 = d04p2 "day04.txt"
    }

data Record = Record
  { name :: !Int,
    left :: ![Int],
    right :: ![Int]
  }
  deriving (Show)

readRecord :: String -> Record
readRecord l =
  let split = words l
      name = read . filter isDigit . head $ drop 1 split
      numbers = drop 2 split
      left = map read . takeWhile (/= "|") $ numbers
      right = map read . tail . dropWhile (/= "|") $ numbers
   in Record {name, left, right}

points :: Record -> Int
points (Record {name, left, right}) =
  let intersection = filter (member right) left
   in if null intersection
        then 0
        else 2 ^ (length intersection - 1)

intersections :: Record -> Int
intersections (Record {left, right}) = length $ filter (member right) left

member :: (Eq a) => [a] -> a -> Bool
member ls thing = any (== thing) ls
