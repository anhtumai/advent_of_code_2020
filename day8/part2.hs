{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/8
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      pairs = map parse lines
      manyPairs = getListOfEdittedPairs pairs
      answer = bruteForce manyPairs
  print answer

-- Replace 1 tuple with key "nop" with key "jmp" and vice versa. Return all permutations.
getListOfEdittedPairs :: [(String, String)] -> [[(String, String)]]
getListOfEdittedPairs pairs = foldl f [] $ zip [0 ..] pairs
  where
    f accum (index, (key, value))
      | key == "nop" = accum ++ [take index pairs ++ ("jmp", value) : drop (index + 1) pairs]
      | key == "jmp" = accum ++ [take index pairs ++ ("nop", value) : drop (index + 1) pairs]
      | otherwise = accum

bruteForce :: [[(String, String)]] -> Int
bruteForce [] = 0
bruteForce (pairs : pairss)
  | isJust x = fromJust x
  | otherwise = bruteForce pairss
  where
    x = compute pairs 0 [] 0

-- Compute final value. If an instruction is repeated, which means an infinite loop, return Nothing.
-- @param [(String, String)] pairs: list of instructions (exp: [("nop","+1"),("acc","-1"),...])
-- @param Int current: current index of instruction list
-- @param [Int] executedStates: list of executed instructions
-- @param Int result: current result
-- @return Maybe Int: final value
compute :: [(String, String)] -> Int -> [Int] -> Int -> Maybe Int
compute pairs current executedStates result
  | current `elem` executedStates = Nothing
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

parse :: String -> (String, String)
parse s = (head fields, last fields)
  where
    fields = splitOn " " s
