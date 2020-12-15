{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

preamble = 25

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      nums = reverse $ map (\x -> read x :: Int) lines
      answer = func nums
  print answer

func :: [Int] -> Int
func (x : xs)
  | checkValid x (sort $ take preamble xs) = func xs
  | otherwise = x

-- check if given number is the sum of 2 elements in the list (assume that the list is sorted)
checkValid :: Int -> [Int] -> Bool
checkValid _ xs | length xs < 2 = False
checkValid num (x : xs)
  | x + last xs == num = True
  | x + last xs > num = checkValid num $ init (x : xs)
  | x + last xs < num = checkValid num xs
