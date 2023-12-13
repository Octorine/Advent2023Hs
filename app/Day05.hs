module Day05 (day05) where

import Data.Either
import Data.List
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  Short description of the problem.
-- >>> d05p1 "day05-ex.txt"
-- "35"
d05p1 filename = do
  input <- datafile filename
  let Right (seeds, maps) = runParser parseInput () "input" input
  let mappedSeeds = foldl' (\ss m -> map (\s -> applyMap m s) ss) seeds maps
  return . show $ minimum mappedSeeds

-- | Part 2.  Short description of the problem.
-- >>> d05p2 "day05-ex.txt"
-- "46"
d05p2 filename = do
  input <- datafile filename
  let Right (seeds, maps) = runParser parseInput () "input" input
  let ranges = elaborate $ seeds
  let mappedSeeds = foldl' (\rs m -> (concat (map (\r -> r `seq` applyMapToRange m r) rs))) ranges maps
  return . show $ minimum . map start $ mappedSeeds

day05 :: Day
day05 =
  Day
    { dayName = "05",
      dayPart1 = d05p1 "day05.txt",
      dayPart2 = d05p2 "day05.txt"
    }

data Map = Map
  { name :: String,
    mappings :: [Mapping]
  }
  deriving (Show)

data Mapping = Mapping
  { srcStart :: Integer,
    srcEnd :: Integer,
    destStart :: Integer
  }
  deriving (Show)

parseInput = do
  seeds <- parseSeeds
  newline
  newline
  maps <- sepBy parseMap newline
  return (seeds, maps)

parseSeeds = do
  string "seeds: "
  seeds <- sepBy parseNumber (char ' ')
  return seeds

parseMap = do
  from <- many1 letter
  char '-'
  string "to"
  char '-'
  to <- many1 letter
  string " map:"
  newline
  mappings <- many1 $ do
    mapping <- parseMapping
    newline
    return mapping
  return Map {name = from ++ "-to-" ++ to, mappings = mappings}

parseMapping = do
  dstStart <- parseNumber
  char ' '
  srcStart <- parseNumber
  char ' '
  rngLength <- parseNumber
  return
    Mapping
      { srcStart = srcStart,
        srcEnd = srcStart + rngLength - 1,
        destStart = dstStart
      }

parseNumber :: Parsec String () Integer
parseNumber = read <$> many1 digit

applyMap m seed = go (mappings m)
  where
    go [] = seed
    go (mm : mms) =
      if seed >= srcStart mm && seed <= srcEnd mm
        then seed - srcStart mm + destStart mm
        else go mms

data Range = Range
  { start :: Integer,
    end :: Integer
  }
  deriving (Show)

elaborate :: [Integer] -> [Range]
elaborate [] = []
elaborate (range : len : rest) =
  Range
    { start = range,
      end = range + len - 1
    }
    : elaborate rest

simplify :: [Range] -> [Range]
simplify [] = []
simplify (r : rs) =
  if any (`includes` r) rs
    then simplify rs
    else r : simplify (filter (not . includes r) rs)

includes :: Range -> Range -> Bool
includes r1 r2 = start r1 <= start r2 && end r1 >= end r2

applyMapToRange :: Map -> Range -> [Range]
applyMapToRange m r = go (mappings m) r
  where
    go [] r = [r]
    go (m : ms) r =
      let (sr, er, sm, em, shift) =
            (start r, end r, srcStart m, srcEnd m, destStart m - srcStart m)
       in if sm <= sr && er <= em -- [sm, sr, er, em]
      --      < Range >
      --    <  Mapping  >
            then [Range {start = sr + shift, end = er + shift}]
            else
              if sr < sm && sm <= er && er <= em -- [sr, sm, er, em]
              -- < Range >
              --    < Mapping >
                then
                  (go ms Range {start = sr, end = sm - 1})
                    ++ [Range {start = sm + shift, end = er + shift}]
                else
                  if sm <= sr && sr <= em && em < er -- [sm, sr, em, er]
                  --        < Range >
                  --    < Mapping >
                    then
                      [Range {start = sr + shift, end = em + shift}]
                        ++ (go ms Range {start = em + 1, end = er})
                    else
                      if sr < sm && em < er
                        then -- <    Range    >
                        --    < Mapping >

                          (go ms Range {start = sr, end = sm - 1})
                            ++ [Range {start = sm + shift, end = em + shift}]
                            ++ (go ms Range {start = em + 1, end = er})
                        else go ms r

prettyMap m =
  name m
    ++ ":\n"
    ++ (concat $ prettyMapping <$> mappings m)
  where
    prettyMapping mp =
      "    src: "
        ++ show (srcStart mp)
        ++ " - "
        ++ show (srcEnd mp)
        ++ ", shift "
        ++ show (destStart mp - srcStart mp)
        ++ " to "
        ++ show (destStart mp)
        ++ "\n"
