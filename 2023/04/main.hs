module Main where
import System.IO (readFile)
import Data.List.Filter (dropUntil)
import Data.List.Split (splitOn)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents


solve1 :: String -> Int
solve1 =
  sum
  . map ((2^) . (+ (-1)) . length)
  . filter (not . null)
  . nums


solve2 =
  sum
  . cards
  . map length
  . nums

-- Get wins of each card number.
cards :: [Int] -> [Int]
cards [] = []
cards (x:xs) = 1 + sum (take x . cards $ xs) : cards xs

-- Parse data into lists of only the matching numbers.
nums :: String -> [[Int]]
nums =
  map ( (\[h,t] -> filter (`elem` t) h)
        . map (map read . words)
        . splitOn "|"
        . dropUntil (==':'))
  . lines
