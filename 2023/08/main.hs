module Main where
import Data.List (cycle)
import Data.List.Split (splitOneOf)
import qualified Data.Map as M
import System.IO (readFile)

main = do
  contents <- readFile "input.txt"
  print . solve1 $ contents
  print . solve2 $ contents


solve1 :: String -> Int
solve1 str = getSteps mp dirs (=="ZZZ") "AAA"
  where
    (dirs, mp) = parseData str



solve2 :: String -> Int
solve2 str =
  foldl1 lcm
  . map (getSteps mp dirs ((=='Z') . last))
  . filter ((=='A') . last)
  . M.keys
  $ mp
  where
    (dirs, mp) = parseData str


getSteps ::
  M.Map String (String, String)
  -> [(String, String) -> String]
  -> (String -> Bool)
  ->  String
  ->  Int
getSteps mp dirs goal start =
  fst
  . head
  . dropWhile (not . goal . snd)
  . zip [0..]
  . scanl (\val dir -> dir $ mp M.! val) start
  . cycle
  $ dirs


parseData :: String -> ([(a,a) -> a],
                        M.Map String (String, String))
parseData = (\(seq:_:kv) -> (makeDirs seq, makeMap kv)) . lines


makeDirs :: String -> [(a,a) -> a]
makeDirs = map (\x -> if x == 'L' then fst else snd)


makeMap :: [String] -> M.Map String (String, String)
makeMap =  M.fromList . map ((\[k,l,r] -> (k,(l,r))) . filter (not . null) . splitOneOf "()=, ")
