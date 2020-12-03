{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Debug.Trace
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = init $ splitOn "\n" contents
      steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      slopes = map (countTree list 0) steps
      answer = product slopes
  print answer
  hClose handle

countTree :: [String] -> Int -> (Int, Int) -> Int
countTree [] _ _ = 0
countTree (x : xs) pos (_, stepDown) | stepDown > length xs = if x !! pos == '#' then 1 else 0
countTree (x : xs) pos (stepRight, stepDown)
  | x !! pos == '#' = 1 + countTree (drop (stepDown -1) xs) newPos (stepRight, stepDown)
  | x !! pos == '.' = countTree (drop (stepDown -1) xs) newPos (stepRight, stepDown)
  where
    len = length x
    newPos = calculateNewPos pos stepRight len

calculateNewPos :: Int -> Int -> Int -> Int
calculateNewPos pos step len
  | pos + step < len = pos + step
  | otherwise = calculateNewPos (pos - len) step len
