{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import qualified Data.IntMap.Lazy as Map
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 1 and 2 in https://adventofcode.com/2020/day/15

main = do
  let input = "1,2,16,19,18,0"
      roundPart1 = 2020
      roundPart2 = 30000000
  let answer1 = solve input roundPart1
      answer2 = solve input roundPart2
  print answer1
  print answer2

solve :: String -> Int -> Int
solve input round = next numsMap (last nums) (length nums) (round - length nums)
  where
    nums = map (\x -> read x :: Int) $ splitOn "," input
    numsMap = Map.fromList $ zip nums [1 .. (length nums)] -- (value, the-last-turn-that-value-was-spoken)

next :: Map.IntMap Int -> Int -> Int -> Int -> Int
next _ lastVal _ 0 = lastVal
next map lastVal lastIndex round = next (Map.insert lastVal lastIndex map) newVal (lastIndex + 1) (round - 1)
  where
    index = Map.lookup lastVal map
    newVal = maybe 0 (lastIndex -) index
