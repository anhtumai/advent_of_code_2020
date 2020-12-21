{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/14
main = do
  let list = []
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
compute (Right (key, value) : instructions) mask result = compute instructions mask (Map.union memValueMap result)
  where
    memValuePairs = map (,value) $ getAllMems mask key
    memValueMap = Map.fromList memValuePairs

-- | Get all memories after being overwritten by bitmask
-- getAllMems "000000000000000000000000000000X1001X" 42 -> [26,27,58,59]
getAllMems :: String -> Int -> [Int]
getAllMems mask address = map toDecimal $ generate $ zipWith (curry f) mask (toBin address)
  where
    f :: (Char, Char) -> Char
    f (x, y)
      | x == 'X' = 'X'
      | x == '0' = y
      | otherwise = '1'

-- | Generate all permutations of binary string, which char 'X' can be replaced by '0' or '1'
-- "0X0X" -> ["0000","0001","0100","0101"]
generate :: String -> [String]
generate xs = generate' xs ""

generate' :: String -> String -> [String]
generate' [] string = [string]
generate' (x : xs) string
  | x == 'X' = generate' xs (string ++ "0") ++ generate' xs (string ++ "1")
  | otherwise = generate' xs (string ++ [x])

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
