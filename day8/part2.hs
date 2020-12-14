{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      pairs = map parse lines
      manyPairs = getListOfEdittedPairs pairs
      answer = bruteForce manyPairs
  print answer

getListOfEdittedPairs :: [(String, String)] -> [[(String, String)]]
getListOfEdittedPairs pairs = foldl f [] $ zip [0 ..] pairs
  where
    f accum (index, (key, value))
      | key == "nop" = accum ++ [take index pairs ++ ("jmp", value) : drop (index + 1) pairs]
      | key == "jmp" = accum ++ [take index pairs ++ ("nop", value) : drop (index + 1) pairs]
      | otherwise = accum

bruteForce :: [[(String, String)]] -> Int
bruteForce [] = 0
bruteForce (pairs : manyPairs)
  | isJust x = fromJust x
  | otherwise = bruteForce manyPairs
  where
    x = compute pairs 0 [] 0

compute :: [(String, String)] -> Int -> [Int] -> Int -> Maybe Int
compute pairs current executedStates result
  | elem current executedStates = Nothing
  | current == length pairs = Just result
  | key == "nop" = compute pairs (current + 1) (current : executedStates) result
  | key == "acc" = compute pairs (current + 1) (current : executedStates) (calculate value result)
  | key == "jmp" = compute pairs (calculate value current) (current : executedStates) result
  where
    (key, value) = pairs !! current

calculate :: String -> Int -> Int
calculate eval first
  | operator == '+' = first + second
  | operator == '-' = first - second
  where
    operator = head eval
    second = read (drop 1 eval) :: Int

-- "nop +0" -> ("nop","+0", False)
parse :: String -> (String, String)
parse s = (head fields, last fields)
  where
    fields = splitOn " " s
