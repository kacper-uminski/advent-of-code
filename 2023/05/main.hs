module Main where
import System.IO
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (find, minimumBy, sortBy)
import Data.List.Split (chunksOf, linesBy)
import Distribution.Simple.Utils (xargs)

main = do
  contents <- readFile "input.txt"
  --mapM_ print . lines $ contents
  print . solve1 $ contents
  print . solve2 $ contents
  

solve1 str = minimum . getLocations maps $ seeds
  where
    (seeds, rest) = parseData str
    maps = map (map getRange) rest
 

solve2 str = minimumBy (compare `on` snd) $ zip <*> getLocations maps $ seeds
  where
    (ranges, rest) = parseData str
    maps = map (map getRange) rest
    seeds = concatMap (\[s, n] -> [s..s+n-1]) . chunksOf 2 $ ranges


getLocations :: Foldable t => t [(Int, Int, Int)] -> [Int] -> [Int]
getLocations maps =  map (\x -> foldl translate x maps)


translate :: Int -> [(Int, Int, Int)] -> Int
translate x [] = x
translate x ((s, d, l):xs) = if s <= x && x < s+l then d+x-s else translate x xs

getRange :: [Int] -> (Int, Int, Int)
getRange [d,s,l] = (s, d, l)

parseData :: String -> ([Int], [[[Int]]])
parseData str = (map read . tail . head $ seeds,
                 map (map (map read)  . tail) rest)
  where seeds:rest = linesBy null . map words . lines $ str
