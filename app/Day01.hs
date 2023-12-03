module Day01 (day01) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)
import Data.Char (isDigit)
datafile filename = (getDataFileName $ "data/" ++ filename) >>= readFile 

{- | Part 1.  Input is a list of lines containing letters and numbers.
Ignoring the letters, take the first and last digit from each line,
make them into a two-digit number, and add them all up.

>>> d01p1"day01-ex.txt"
"142"
-}
d01p1 filename = do
  input <- datafile filename
  let result  = sum . map (\line -> read (take 1 line) * 10 + read (take 1 (reverse line))) . map (filter isDigit) $ lines input
  return $ show result

{- | Part 2. Same as before, but some of the letters are digits spelled
   out. Now for each line take the first digit or spelled-out digit
   and the last spelled-out digit, then make two-digit numbers and add
   them up like in step one.

>>> d01p2 "day01p2-ex.txt"
"281"
-}
d01p2 filename = do
  input <- datafile filename
  return
    . show
    . sum
    . map (\line -> read (firstDigit digits line) * 10 +
            read (lastDigit digits line))
    $ lines input


day01 :: Day
day01 =
  Day
    { dayName = "01",
      dayPart1 = d01p1 "day01.txt",
      dayPart2 = d01p2 "day01.txt"
    }
digits :: [(String, String)]
digits =
  [("one", "1"),
    ( "two" ,"2"),
    ( "three", "3"),
    ( "four", "4"),
    ( "five", "5"),
    ( "six", "6"),
    ( "seven", "7"),
    ( "eight", "8"),
    ( "nine", "9")]
  

firstDigit :: [(String, String)] -> String -> String
firstDigit digits string = go digits string
  where go _ [] = []
        go [] string = go digits $ tail string
        go (digit: digits) string =
          if isDigit (head string) then
            take 1 string
          else if take (length (fst digit)) string == fst digit then
                 snd digit
               else go digits string
  
lastDigit digits string =
  firstDigit
     (map (\(k, v) -> (reverse k, v)) digits)
     (reverse string)
