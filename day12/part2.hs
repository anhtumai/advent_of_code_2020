{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/12
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let instructions = map (\s -> (head s, read (drop 1 s) :: Int)) $ init $ splitOn "\n" contents
      (x, y) = compute instructions (0, 0) (10, 1)
      answer = abs x + abs y
  print answer
  hClose handle

compute :: [(Char, Int)] -> (Int, Int) -> (Int, Int) -> (Int, Int)
compute [] (x, y) _ = (x, y)
compute ((command, value) : instructions) (x, y) direction
  | command == 'L' = compute instructions (x, y) $ switchLeft direction (div value 90)
  | command == 'R' = compute instructions (x, y) $ switchRight direction (div value 90)
  | command == 'N' = compute instructions (x, y) (directionX, directionY + value)
  | command == 'E' = compute instructions (x, y) (directionX + value, directionY)
  | command == 'S' = compute instructions (x, y) (directionX, directionY - value)
  | command == 'W' = compute instructions (x, y) (directionX - value, directionY)
  | command == 'F' = compute instructions (x + value * directionX, y + value * directionY) direction
  | otherwise = compute instructions (x, y) direction
  where
    (directionX, directionY) = direction

switchLeft :: (Int, Int) -> Int -> (Int, Int)
switchLeft direction 0 = direction
switchLeft (directionX, directionY) time = switchLeft (- directionY, directionX) (time -1)

switchRight :: (Int, Int) -> Int -> (Int, Int)
switchRight direction 0 = direction
switchRight (directionX, directionY) time = switchRight (directionY, - directionX) (time -1)
