module Main where
import System.IO
import Data.List.Split
import Data.Char (isNumber)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents



solve1 =
  sum
  . map fst
  . filter (\(n, v) -> and $ zipWith (>=) [12, 13, 14] v)
  . zip [1..]
  . maxes

solve2 =
  sum
  . map product
  . maxes
  
maxes =
  map ((\x -> map maxVal ["red", "green", "blue"] <*> pure x)
        . concatMap (cubeset . wordsBy (`elem` ",: "))
        . wordsBy (`elem` ";")
        . dropWhile (/=':') )
  . lines 


maxVal :: String -> [(String, Int)] -> Int
maxVal str = maximum . map (\(x,y) -> if str == x  then y else 0)

cubeset [] = []
cubeset (x:xs:xxs) = (xs, read x :: Int):cubeset xxs
