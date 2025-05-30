module Main where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Game.Advent

main :: IO ()
main = do
  putStrLn "Advent of Code, 2023"
  runDay day01
  runDay day02
  runDay day03
  runDay day04
  runDay day05
  runDay day06
  runDay day07
  runDay day08
  runDay day09
  runDay day10
  runDay day11
  runDay day12
