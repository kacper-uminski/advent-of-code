module Main where
import System.IO
import Data.Char
import Data.List (replicate)
import Data.List.Split (splitWhen)

main = do
  contents <- readFile "input.txt"
  --mapM_ print . solve1 $ contents
  print . solve1 $ contents



solve1 = sum . numbers 

--numbers :: String -> [Int]
numbers lst =
  map (read . map fst)
  . filter (any (any (`notElem` "1234567890.") . snd))
  . filter (not . null)
  . splitWhen (not . isNumber . fst)
  $ zip (concat . padDots $ lst) (getNeighbors lst)

getNeighbors = foldr (zipWith (:)) (repeat []) . (offsets <*>) . pure

--rotate :: Int -> [a] -> [a]
rotate x st = take (length st) $ drop (negate x `mod` length st) $ cycle st

padDots :: String -> [String]
padDots x = dotList:middle++[dotList]
  where
    middle = map (\s -> '.':s ++ ".") . lines $ x 
    dotList = replicate (length $ head middle) '.' 

offsets = (\(a,b) -> concat . rotate b . map (rotate a) . padDots) <$> [(a, b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]
