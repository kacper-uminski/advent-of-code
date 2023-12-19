module Main where
import System.IO (readFile)
import Data.List (find)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents


solve1 :: String -> Int
solve1 = foldl translate 0


solve2 :: String -> Int
solve2 =
  fst
  . last
  . takeWhile ((>=0) . snd)
  . zip [1..]
  . scanl translate 0


translate :: Num a => a -> Char -> a
translate n '(' = n+1
translate n ')' = n-1
translate n _ = n
