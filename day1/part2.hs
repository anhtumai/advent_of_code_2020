{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.Maybe
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  let sortedList = sort list
  let answer = getAnswer $ getTrio 2020 sortedList
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

getTrio :: Int -> [Int] -> Maybe (Int, Int, Int)
getTrio sum xs | length xs <= 2 = Nothing
getTrio sum (x : xs)
  | isJust pair = Just (x, y, z)
  | otherwise = getTrio sum xs
  where
    pair = getPair (sum - x) xs
    Just (y, z) = pair

getAnswer :: Maybe (Int, Int, Int) -> String
getAnswer Nothing = "There are no pairs satisfying the condition"
getAnswer (Just (x, y, z)) = "The answer is " ++ show (x * y * z)
