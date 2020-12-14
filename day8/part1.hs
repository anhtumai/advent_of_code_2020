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
  let pairs = map parse lines

  let answer = compute pairs 0 [] 0
  print answer

compute :: [(String, String)] -> Int -> [Int] -> Int -> Int
compute pairs current executedStates result
  | elem current executedStates = result
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
