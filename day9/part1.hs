{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

preamble = 25

-- Problem: Part 1 in https://adventofcode.com/2020/day/9
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      nums = reverse $ map (\x -> read x :: Int) lines
      answer = find nums
  print answer

-- Find first number which is not sum of 2 elements inside preamble length
find :: [Int] -> Int
find (x : xs)
  | checkValid x (sort $ take preamble xs) = func xs
  | otherwise = x

-- check if given number is the sum of 2 elements in the list (assume that the list is sorted)
checkValid :: Int -> [Int] -> Bool
checkValid _ xs | length xs < 2 = False
checkValid num (x : xs)
  | x + last xs == num = True
  | x + last xs > num = checkValid num $ init (x : xs)
  | x + last xs < num = checkValid num xs
