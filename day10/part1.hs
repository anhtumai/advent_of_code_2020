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
      nums = 0 : sort (map (\x -> read x :: Int) lines)
      (one, two, three) = func nums
      answer = one * three

  print answer

func :: [Int] -> (Int, Int, Int)
func [_] = (0, 0, 1)
func (num : nums)
  | head nums - num == 1 = (x + 1, y, z)
  | head nums - num == 2 = (x, y + 1, z)
  | otherwise = (x, y, z + 1)
  where
    (x, y, z) = func nums