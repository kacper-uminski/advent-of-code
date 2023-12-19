module Main where
import System.IO (readFile)


main = do
  contents <- readFile "input.txt"
  mapM_ print . solve1 $ contents


solve1 = lines


data Dir = N | S | E | W deriving (Eq, Show)
