module Main where
import Data.List.Split (splitOn)
import System.IO (readFile)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents


--solve1 :: String -> Int
solve1 =
  sum
  . map ((\[l,w,h] -> let xs = [2*l*w, 2*w*h, 2*h*l] in sum xs + minimum xs)
         . map (read @Int)
         . splitOn "x")
  . lines



--solve2 :: String -> Int


