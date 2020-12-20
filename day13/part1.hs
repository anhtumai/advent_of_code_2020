{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 1 in https://adventofcode.com/2020/day/13
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let info = init $ splitOn "\n" contents
      start = read (head info) :: Int
      buses = map (\x -> read x :: Int) $ filter (/= "x") $ splitOn "," (last info)
      waitingTimes = map (\bus -> bus * (div start bus + 1) - start) buses

      index = foldl (\res (i, val) -> if val <= waitingTimes !! res then i else res) 0 $ zip [0 ..] waitingTimes

      answer = buses !! index * waitingTimes !! index

  print answer
  hClose handle
