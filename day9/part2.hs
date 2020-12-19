{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/9
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      nums = map (\x -> read x :: Int) lines
      x = sort $ getSequence nums 22406676 []
      answer = head x + last x
  print answer
  hClose handle

-- | Get a sequence of numbers which sum up to the input number
-- @param [Int] xs
-- @param Int num: input number
-- @param [Int] stack
-- @return [Int]: list of number sum up to num
getSequence :: [Int] -> Int -> [Int] -> [Int]
getSequence (x : xs) num stack
  | currentSum < num = getSequence xs num (stack ++ [x])
  | currentSum == num = stack
  | currentSum > num = getSequence xs num (pop (stack ++ [x]) num)
  where
    currentSum = sum stack

-- return a new list which sum of element is less than a given number
pop :: [Int] -> Int -> [Int]
pop xs num
  | sum xs > num = pop (drop 1 xs) num
  | sum xs <= num = xs
