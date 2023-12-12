{-# LANGUAGE NamedFieldPuns #-}
module Day02 (day02) where
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)
import Text.Parsec
import Text.Parsec.Char

datafile filename = (getDataFileName $ filename) >>= readFile 

{- | Part 1.  Short description of the problem.
>>> d02p1 "day02-ex.txt"
"8"
-}
d02p1 filename = do
  input <- datafile filename
  let games = runParser gamesParser () "" input
  case games of
    Left e -> error $ show  e
    Right g ->  return
                . show
                . sum
                . map number
                . filter possible  $ g


{- | Part 2.  Short description of the problem.
>>> d02p2 "day02-ex.txt"
"2286"
-}
d02p2 filename = do
  input <- datafile filename
  let games = runParser gamesParser () "" input
  case games of
    Left e -> error $ show  e
    Right g ->  return
                . show
                . sum
                . map power  $ g



day02 :: Day
day02 =
  Day
    { dayName = "02",
      dayPart1 = d02p1 "day02.txt",
      dayPart2 = d02p2 "day02.txt"
    }

data Game = Game { number:: Int, rounds:: [Round] } 
  deriving (Eq, Show)

data Round = Round {red:: !Int, blue:: !Int, green:: !Int}
  deriving (Eq, Show)

gamesParser :: Parsec String () [Game]
gamesParser = sepBy  gameParser newline

gameParser = do
  string "Game "
  num <- read <$> many1 digit
  string ": "
  rounds <- sepBy roundParser  (string "; ")
  return Game { number = num, rounds }

roundParser = do
  picks <- sepBy pickParser (string ", ") 
  return $ mconcat picks

pickParser = do
  num <- read <$> many1 digit 
  space
  color <- many1 letter
  return $ case color of
    "red" -> Round { red = num, green = 0, blue = 0}
    "green" -> Round {red = 0, green = num , blue = 0 }
    "blue" -> Round {red = 0 , green = 0, blue = num}
  

instance Monoid Round where
  mempty =     Round {red = 0, green = 0, blue = 0}

instance Semigroup Round where
  r1 <> r2 =
    Round {red = red r1 + red r2,
            green = green r1 + green r2,
            blue = blue r1 + blue r2}
possible :: Game -> Bool
possible (Game {rounds}) = all (\r -> red r <= 12 && green r <= 13 && blue r <= 14) rounds

power :: Game -> Int
power (Game {rounds}) =
       let minRed = maximum . map red $ rounds
           minBlue = maximum . map blue $ rounds
           minGreen = maximum . map green $ rounds
       in minRed * minBlue * minGreen
