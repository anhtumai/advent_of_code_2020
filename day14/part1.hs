{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import System.IO

-- Problem: Part 1 in https://adventofcode.com/2020/day/14
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let info = init $ splitOn "\n" contents
      instructions = map parse info
      mems = compute instructions "" Map.empty
      answer = foldl (\acc (_, x) -> acc + x) 0 $ Map.toList mems
  print answer
  hClose handle

compute :: [Either String (Int, Int)] -> String -> Map.Map Int Int -> Map.Map Int Int
compute [] _ result = result
compute (Left mask : instructions) _ result = compute instructions mask result
compute (Right (key, value) : instructions) mask result = compute instructions mask (Map.insert key x result)
  where
    x = bitmaskOverwrite mask value

-- | Parse instruction
-- "mask = 00X000X0001X111111101X1111XX11X001XX" -> Left "000000000000000000000000000000X1001X"
-- "mem[8] = 42" -> Right (8,42)
parse :: String -> Either String (Int, Int)
parse s
  | key == "mask" = Left value
  | otherwise = Right (read (init $ drop 4 key) :: Int, read value :: Int)
  where
    x = splitOn " = " s
    key = head x
    value = last x

-- | Get new value which is overwritten by bitmask
-- bitmaskOverwrite "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11 -> 73
bitmaskOverwrite :: [Char] -> Int -> Int
bitmaskOverwrite mask n = toDecimal $ zipWith (curry f) mask (toBin n)
  where
    f (m, x)
      | m == 'X' = x
      | otherwise = m

-- | Convert binary string to decimal number
toDecimal :: [Char] -> Int
toDecimal [] = 0
toDecimal (x : xs)
  | x == '1' = 2 ^ length xs + toDecimal xs
  | otherwise = toDecimal xs

-- | Convert decimal Int to a binary string with length 36
toBin :: Int -> [Char]
toBin n = ['0' | i <- [1 .. (36 - length x)]] ++ x
  where
    x = toBin' n

toBin' :: Int -> [Char]
toBin' 0 = []
toBin' n = toBin' (div n 2) ++ show (mod n 2)
