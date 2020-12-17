{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

preamble = 25

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let lines = splitOn "\n" $ init contents
      nums = sort (map (\x -> read x :: Int) lines)
      adapters = 0 : nums ++ [last nums + 3]
      answer = last $ getOptions adapters [1]
  print answer

-- Solution:
-- Find number of options to reach each target.
-- For example: [0,1,4,5,6,7,...] -> [1,1,1,1,2,4,...]

-- Target:
-- 1: 1 option (0 + 1 = 1)
-- 4: 1 option (1 + 3 = 4)
-- 5: 1 option (4 + 1 = 5)
-- 6: 2 options = 1 (4) + 1 (5)
-- 7: 4 options = 2 (6) + 1 (5) + 1(4)

-- Use dynamic programming to store options for each target

-- getOptions [0,1,4,5,6,7,...] [1] -> [1,1,1,1,2,4,...]
getOptions :: [Int] -> [Int] -> [Int]
getOptions [] _ = []
getOptions nums options
  | lenOptions < length nums = getOptions nums $ options ++ [option]
  | otherwise = options
  where
    lenOptions = length options
    option = foldl f 0 [1, 2, 3]
    f acc x = if isJust index then acc + (options !! fromJust index) else acc
      where
        index = elemIndex (nums !! lenOptions - x) nums
