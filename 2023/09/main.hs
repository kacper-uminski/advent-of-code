module Main where
import System.IO (readFile)
import Data.List (unfoldr)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents

solve1 =
  sum
  . map (sum . map last . getDiffList)
  . lines

solve2 =
  sum
  . map (foldr1 (-) . map head . getDiffList)
  . lines


getDiffList =
  unfoldr getDiffs
  . map (read @Int)
  . words 

getDiffs xs =
  if all (==0) xs
  then Nothing
  else Just (xs, mapAdjacent (flip (-)) xs)

mapAdjacent fn =
  zipWith fn <*> tail 
