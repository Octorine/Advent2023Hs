module Main where
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Game.Advent

main :: IO ()
main = do
  putStrLn "Advent of Code, 2023"
  runDay day01
  runDay day02
  runDay day03
  runDay day04

