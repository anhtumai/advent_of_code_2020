{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

-- Problem: Part 2 in https://adventofcode.com/2020/day/13
main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let info = init $ splitOn "\n" contents
      start = read (head info) :: Int
      buses = map (\(i, val) -> (i, read val :: Int)) $ filter (\(i, val) -> val /= "x") $ zip [0 ..] $ splitOn "," (last info)
      answer = calculate buses

  print answer
  hClose handle

-- According to Chinese Remainder Theorem:
-- X `mod` n1 = r1
-- X `mod` n2 = r2
-- ...
-- Formula: X = sum of r(i)*N(i)*x(i) (mod N)
-- where:
-- r(i): remainder
-- N = n1*n2*...
-- N(i) = N / n(i)
-- x(i): inverse of N(i) in mod n(i)
-- Note: this theorem is applied only if gcd(n1,n2,..) is equal to 1. Fortunately, this condition is true in this exercise input.
-- Find more about CRT at: https://www.youtube.com/watch?v=zIFehsBHB8o

-- | Calculate the smallest number X which satisfies X `mod` n(i) = r(i)
-- @param [(Int,Int)]: [(r(i),n(i))]
-- @return [Int]: smallest number X satisfying multiple modulus conditions
calculate :: [(Int, Int)] -> Int
calculate pairs = (div bigX bigN + 1) * bigN - bigX
  where
    bigN = foldl (\acc (r, n) -> acc * n) 1 pairs
    bigX = foldl (\acc (r, n) -> acc + r * div bigN n * getInverse (div bigN n) n) 0 pairs

-- | Give integer n and m where n*x = 1 (mod m), find x = ? (mod m)
getInverse :: Int -> Int -> Int
getInverse n m = foldl (\res x -> if mod (n * x) m == 1 then x else res) 1 [1 .. m]
