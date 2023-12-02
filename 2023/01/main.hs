module Main where
import System.IO

main = do
  contents <- readFile "input.txt"
  print . decode $ contents

decode :: String -> Int
decode = sum . map (read . digits) . lines

nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] ++ [[c] | c <- ['0'..'9']]

firstDigit :: String -> Char
firstDigit x = case filter (\c -> all id $ zipWith (==) c x) nums of
  [] -> firstDigit . tail $ x
  y:ys -> toNumChar y

lastDigit :: String -> Char
lastDigit x = case filter (\c -> all id $ zipWith (==) c (reverse x)) (map reverse nums) of
  [] -> lastDigit . init $ x 
  y:ys -> toNumChar . reverse $ y

digits :: String -> String
digits str = [firstDigit str, lastDigit str]

toNumChar :: String -> Char
toNumChar x = case x of
  "zero" -> '0'
  "one" -> '1'
  "two" -> '2'
  "three" -> '3'
  "four" -> '4'
  "five" -> '5'
  "six" -> '6'
  "seven" -> '7'
  "eight" -> '8'
  "nine" -> '9'
  "0" -> '0'
  "1" -> '1'
  "2" -> '2'
  "3" -> '3'
  "4" -> '4'
  "5" -> '5'
  "6" -> '6'
  "7" -> '7'
  "8" -> '8'
  "9" -> '9'
