module Day07 (day07) where

import Data.Char
import Data.List
import Data.Ord
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  Short description of the problem.
-- >>> d07p1 "day07-ex.txt"
-- "6440"
d07p1 filename = do
  input <- datafile filename
  return . show . winnings . parseInput $ input

-- | Part 2.  Short description of the problem.
-- >>> d07p2 "day07-ex.txt"
-- "5905"
d07p2 filename = do
  input <- datafile filename
  return . show . winnings . map convertHand . parseInput $ input

day07 :: Day
day07 =
  Day
    { dayName = "07",
      dayPart1 = d07p1 "day07.txt",
      dayPart2 = d07p2 "day07.txt"
    }

data Hand = Hand {rank :: Rank, cards :: String, bid :: Int}
  deriving (Show)

data Rank = HighCard | Pair | TwoPair | Three | FullHouse | Four | Five
  deriving (Show, Ord, Eq)

parseInput :: String -> [Hand]
parseInput input =
  map parseHand $ lines input
  where
    parseHand line =
      let [cards, bidTxt] = words line
       in Hand {rank = findRank cards, cards = cards, bid = read bidTxt}

findRank cards =
  case (reverse . sort . map length . group . sort $ cards) of
    [1, 1, 1, 1, 1] -> HighCard
    [2, 1, 1, 1] -> Pair
    [2, 2, 1] -> TwoPair
    [3, 1, 1] -> Three
    [3, 2] -> FullHouse
    [4, 1] -> Four
    [5] -> Five

pips :: Char -> Int
pips c =
  if c >= '0' && c <= '9'
    then digitToInt c
    else case c of
      'T' -> 10
      'J' -> 11
      'Q' -> 12
      'K' -> 13
      'A' -> 14

instance Ord Hand where
  compare h1 h2 =
    case compare (rank h1) (rank h2) of
      EQ -> compare (pips <$> cards h1) (pips <$> cards h2)
      _ -> compare (rank h1) (rank h2)

instance Eq Hand where
  h1 == h2 = cards h1 == cards h2

winnings :: [Hand] -> Int
winnings hands =
  let sorted = zip [1 ..] (sort hands)
   in sum . map (\(index, hand) -> index * bid hand) $ sorted

convertHand :: Hand -> Hand
convertHand h =
  let noJokers = filter (/= 'J') $ cards h
      mostPopular =
        head
          . maximumBy (comparing length)
          . group
          . sort
          $ noJokers
      newRank =
        if cards h == "JJJJJ"
          then Five
          else findRank . mapJokers mostPopular $ cards h
      newCards = mapJokers '0' $ cards h
      mapJokers c cs = (\x -> if x == 'J' then c else x) <$> cs
   in Hand {cards = newCards, rank = newRank, bid = bid h}
