{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      nums = map (\x -> read x :: Int) lines
      x = sort $ func nums 22406676 []
      answer = head x + last x
  print answer

-- use stack to store value
func :: [Int] -> Int -> [Int] -> [Int]
func [] _ stack = stack
func (x : xs) num stack
  | currentSum < num = func xs num (stack ++ [x])
  | currentSum == num = stack
  | currentSum > num = func xs num (pop (stack ++ [x]) num)
  where
    currentSum = sum stack

-- return a new list which sum of element is less than a given number
pop :: [Int] -> Int -> [Int]
pop xs num
  | sum xs > num = pop (drop 1 xs) num
  | sum xs <= num = xs
