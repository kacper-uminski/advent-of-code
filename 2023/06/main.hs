module Main where
import Data.Char (isNumber)
import Data.List (transpose)
import System.IO

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents


solve1 =
  product
  . map (\(t, d) -> length [distance t x | x <- [1..t-1], distance t x > d])
  . (\[times, dist] -> zip times dist)
  . map (map (read @Int) . tail . words)
  . lines

solve2 =
  (\[t, d] -> length [distance t x | x <- [1..t-1], distance t x > d])
  . map (read @Int . filter isNumber)
  . lines


distance total waitTime = waitTime * (total - waitTime)
