module Day08 (day08) where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Game.Advent
import Paths_Advent2023Hs (getDataFileName)
import Text.Parsec
import Text.Parsec.Char

datafile filename = (getDataFileName $ filename) >>= readFile

-- | Part 1.  First line of input is a list of L and R.  Subsequent
--   lines look like "AAA == (BBB,CCC)".  Start with AAA and go L or R
--   according to the first line (wrappeng around when you get to the
--   end), until you get to ZZZ, returning the number of steps.
--
-- >>> d08p1 "day08-ex.txt"
-- "2"
d08p1 filename = do
  input <- datafile filename
  let Right (directions, nodes) = runParser parseInput () "input" input
  let dict = M.fromList . map (\n -> (name n, (left n, right n))) $ nodes
  return . show $ measure dict directions "AAA" "ZZZ"

-- | Part 2.  Short description of the problem.
-- >>> d08p2 "day08-ex.txt"
-- "TODO"
d08p2 filename = do
  input <- datafile filename
  let Right (directions, nodes) = runParser parseInput () "input" input
  let dict = M.fromList . map (\n -> (name n, (left n, right n))) $ nodes
  let starts = filter ((== 'A') . last . name) nodes
  -- return . show $ measurePar dict directions ( name <$> starts)
  let ends = filter ((== 'Z') . last . name) nodes
  let paths = getAllPaths dict directions starts ends
  return . show . foldl' lcm 1 . mapMaybe (\(_, _, n) -> n) $ paths

day08 :: Day
day08 =
  Day
    { dayName = "08",
      dayPart1 = d08p1 "day08.txt",
      dayPart2 = d08p2 "day08.txt"
    }

data Node = Node
  { name :: String,
    left :: String,
    right :: String
  }
  deriving (Show)

parseInput :: Parsec String () (String, [Node])
parseInput = do
  directions <- many1 (oneOf "LR")
  newline
  newline
  nodes <- sepBy parseNode newline
  return (directions, nodes)

parseNode = do
  name <- many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ124567890"
  string " = ("
  left <- many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ124567890"
  string ", "
  right <- many1 $ oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ124567890"
  char ')'
  return $ Node {name = name, left = left, right = right}

measure :: M.Map String (String, String) -> String -> String -> String -> Int
measure dict directions start end = go start (cycle directions) 0
  where
    go node dir acc =
      acc `seq`
        ( if node == end
            then acc
            else
              let Just (left, right) = M.lookup node dict
               in go
                    (if head dir == 'L' then left else right)
                    (tail dir)
                    (acc + 1)
        )

tryMeasure :: M.Map String (String, String) -> String -> String -> String -> Maybe Integer
tryMeasure dict directions start end = go start (cycle directions) 0 (length directions) S.empty
  where
    go node dir acc dirLength trail =
      acc `seq`
        ( if node == end
            then Just acc
            else
              if S.member (node, take dirLength dir) trail
                then Nothing
                else
                  let Just (left, right) = M.lookup node dict
                      newSet = S.insert (node, take dirLength dir) trail
                   in newSet `seq`
                        go
                          (if head dir == 'L' then left else right)
                          (tail dir)
                          (acc + 1)
                          dirLength
                          newSet
        )

measurePar :: M.Map String (String, String) -> String -> [String] -> Int
measurePar dict directions starts = go starts (cycle directions) 0
  where
    go nodes dir acc =
      if all ((== 'Z') . last) nodes
        then acc
        else
          acc `seq`
            ( let selector = if head dir == 'L' then fst else snd
                  nexts =
                    map (\n -> selector . fromJust $ M.lookup n dict) nodes
               in go nexts (tail dir) (1 + acc)
            )

getAllPaths dict dir starts ends = do
  s <- name <$> starts
  e <- name <$> ends
  return (s, e, tryMeasure dict dir s e)
