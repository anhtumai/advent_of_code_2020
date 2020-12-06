{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Data.List
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
count s = count' s "" 0

count' :: String -> String -> Int -> Int
count' [] _ c = c
count' (x : xs) ys c
  | x /= '\n' && notElem x ys = count' xs (x : ys) (c + 1)
  | otherwise = count' xs ys c
