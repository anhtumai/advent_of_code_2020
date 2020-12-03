{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import System.IO

main = do
  let list = []
  handle <- openFile "day1_input.txt" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  let sortedList = sort list
  let answer = getAnswer $ getPair 2020 sortedList
  print answer
  hClose handle

f :: [String] -> [Int]
f = map read

getPair :: Int -> [Int] -> Maybe (Int, Int)
getPair sum [] = Nothing
getPair sum [x] = Nothing
getPair sum (x : xs)
  | x + last xs < sum = getPair sum xs
  | x + last xs > sum = getPair sum (x : init xs)
  | otherwise = Just (x, last xs)

getAnswer :: Maybe (Int, Int) -> String
getAnswer Nothing = "There are no pairs satisfying the condition"
getAnswer (Just (x, y)) = "The answer is " ++ show (x * y)
