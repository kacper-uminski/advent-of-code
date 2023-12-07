module Main where
import System.IO
import Data.List (group, sort, sortBy)

main = do
  contents <- readFile "input.txt"
  print . solve False $ contents
  print . solve True $ contents


solve b =
  sum
  . zipWith (*) [1..]
  . map (\(a,b,c) -> c)
  . sort
  . map ((\[hand, bid] -> (getType
                           . sortBy (flip compare)
                           . map (categorize b)
                           . group
                           . sort $ hand,
                           map (card b) hand,
                           read @Int bid))
         . words)
  . lines

categorize :: Bool -> String -> (Int, Int)
categorize b str = (length str, card b . head $ str)

card :: Bool -> Char -> Int
card b a = case a of
              'A' -> 14
              'K' -> 13
              'Q' -> 12
              'J' -> if b then 1 else 11
              'T' -> 10
              x -> read @Int [x]

getType :: [(Int, Int)] -> HandType
getType a = case sortBy (flip compare) a of
  [(5,_)] -> FiveOfAKind
  [(4,_), (1,1)] -> FiveOfAKind
  [(4,1), (1,_)] -> FiveOfAKind
  [(4,_), (1,_)] -> FourOfAKind
  [(3,_), (2,1)] -> FiveOfAKind
  [(3,1), (2,_)] -> FiveOfAKind
  [(3,_), (2,_)] -> FullHouse
  [(3,_), (1,_), (1,1)] -> FourOfAKind
  [(3,1), (1,_), (1,_)] -> FourOfAKind
  [(3,_), (1,_), (1,_)] -> ThreeOfAKind
  [(2,_), (2,1), (1,_)] -> FourOfAKind
  [(2,_), (2,_), (1,1)] -> FullHouse
  [(2,_), (2,_), (1,_)] -> TwoPair
  [(2,_), (1,_), (1,_), (1,1)] -> ThreeOfAKind
  [(2,1), (1,_), (1,_), (1,_)] -> ThreeOfAKind
  [(2,_), (1,_), (1,_), (1,_)] -> OnePair
  [(1,_), (1,_), (1,_), (1,_), (1,1)] -> OnePair
  [(1,_), (1,_), (1,_), (1,_), (1,_)] -> HighCard


data HandType =
  HighCard |
  OnePair |
  TwoPair |
  ThreeOfAKind |
  FullHouse |
  FourOfAKind |
  FiveOfAKind
  deriving (Eq, Ord, Show)
