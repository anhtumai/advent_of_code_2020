{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import System.IO

-- Problem: Part 1 in https://adventofcode.com/2020/day/8
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
  let pairs = map parse lines

  let answer = compute pairs 0 [] 0
  print answer

-- Compute final value until an instruction is repeated
-- @param [(String, String)] pairs: list of instructions (exp: [("nop","+1"),("acc","-1"),...])
-- @param Int current: current index of instruction list
-- @param [Int] executedStates: list of executed instructions
-- @param Int result: current result
-- @return Int: final value
compute :: [(String, String)] -> Int -> [Int] -> Int -> Int
compute pairs current executedStates result
  | current `elem` executedStates = result
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
