{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = init $ splitOn "\n" contents
  let ids = map getSeatID list
      sortedIds = sort ids
  let answer = findMissingSeat sortedIds
  print answer
  hClose handle

separate :: String -> Char -> Char -> String
separate [] _ _ = ""
separate (x : xs) upper lower
  | x == upper || x == lower = x : separate xs upper lower
  | otherwise = separate xs upper lower

getNum :: String -> Char -> Int -> Int -> Int
getNum [] _ min _ = min
getNum (x : xs) upper min max
  | x == upper = getNum xs upper (average + 1) max
  | otherwise = getNum xs upper min average
  where
    average = div (min + max) 2

getSeatID :: String -> Int
getSeatID s = row * 8 + column
  where
    row = getNum (separate s 'F' 'B') 'B' 0 127
    column = getNum (separate s 'R' 'L') 'R' 0 7

findMissingSeat :: [Int] -> Int
findMissingSeat [x] = x + 1
findMissingSeat (x : xs)
  | head xs - x > 1 = x + 1
  | otherwise = findMissingSeat xs
