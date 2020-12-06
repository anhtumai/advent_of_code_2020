{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List (sort)
import Data.List.Split
import System.IO

main = do
  let list = []
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let list = splitOn "\n\n" contents

  let answer = foldl (\acc x -> acc + count x) 0 list

  print answer

count :: String -> Int
count xss = length (intersectList (splitOn "\n" xss))

intersectSorted :: Ord a => [a] -> [a] -> [a]
intersectSorted (x : xs) (y : ys)
  | x == y = x : intersectSorted xs ys
  | x < y = intersectSorted xs (y : ys)
  | x > y = intersectSorted (x : xs) ys
intersectSorted _ _ = []

intersect :: Ord a => [a] -> [a] -> [a]
intersect xs ys = intersectSorted (sort xs) (sort ys)

intersectList :: Ord a => [[a]] -> [a]
intersectList [xs] = xs
intersectList (xs : xss) = intersect xs (intersectList xss)
