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
  let f max x
        | max < seatID = seatID
        | otherwise = max
        where
          seatID = getSeatID x
  let answer = foldl f 0 list

  print answer

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
